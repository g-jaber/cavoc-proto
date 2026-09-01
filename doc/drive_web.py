#!/usr/bin/env python3
"""Drive the cavoc page in headless Firefox via Marionette (stdlib only)."""
import base64, json, socket, subprocess, sys, time, os

# Outputs (screenshots, throwaway Firefox profile) go to a temp directory,
# printed at the end - not next to this script.
import tempfile
SCRATCH = tempfile.mkdtemp(prefix="cavoc-drive-")
FIREFOX = "/Applications/Firefox.app/Contents/MacOS/firefox"
URL = "http://localhost:8000/front/index.html"

class Marionette:
    def __init__(self, port=2828, timeout=60):
        deadline = time.time() + timeout
        while True:
            try:
                self.sock = socket.create_connection(("127.0.0.1", port), timeout=5)
                break
            except OSError:
                if time.time() > deadline:
                    raise
                time.sleep(0.5)
        self.sock.settimeout(30)
        self.buf = b""
        self.msgid = 0
        self._read_packet()  # handshake

    def _read_packet(self):
        while b":" not in self.buf:
            self.buf += self.sock.recv(4096)
        length, _, rest = self.buf.partition(b":")
        length = int(length)
        while len(rest) < length:
            rest += self.sock.recv(4096)
        self.buf = rest[length:]
        return json.loads(rest[:length])

    def cmd(self, name, params=None):
        self.msgid += 1
        payload = json.dumps([0, self.msgid, name, params or {}]).encode()
        self.sock.sendall(str(len(payload)).encode() + b":" + payload)
        while True:
            pkt = self._read_packet()
            if isinstance(pkt, list) and pkt[0] == 1 and pkt[1] == self.msgid:
                if pkt[2] is not None:
                    raise RuntimeError(f"{name}: {pkt[2]}")
                return pkt[3]

    def js(self, script, args=None):
        r = self.cmd("WebDriver:ExecuteScript", {"script": script, "args": args or []})
        return r.get("value")

    def shot(self, path):
        r = self.cmd("WebDriver:TakeScreenshot", {"full": False})
        with open(path, "wb") as f:
            f.write(base64.b64decode(r["value"]))

def wait_js(m, script, desc, timeout=30):
    deadline = time.time() + timeout
    while time.time() < deadline:
        v = m.js(script)
        if v:
            return v
        time.sleep(0.4)
    raise SystemExit(f"TIMEOUT waiting for: {desc}")

def main():
    prof = os.path.join(SCRATCH, "ffprof-drive")
    os.makedirs(prof, exist_ok=True)
    ff = subprocess.Popen(
        [FIREFOX, "--headless", "--no-remote", "-marionette",
         "-profile", prof, "--window-size=1500,950", "about:blank"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        m = Marionette()
        m.cmd("WebDriver:NewSession", {})
        m.cmd("WebDriver:Navigate", {"url": URL})

        # 1. sandbox loads: picker populated, one participant card holding the
        #    first example
        state = wait_js(m, """
          const sel = document.getElementById('scenario-select');
          if (!sel || sel.options.length === 0) return null;
          const cards = window.cavocCards || [];
          if (cards.length === 0) return null;
          if (cards[0].editor.getValue().length === 0) return null;
          return { first: sel.value, cards: cards.length,
                   name: cards[0].name,
                   role: document.querySelector('#card-0 .role-badge').textContent,
                   codeLen: cards[0].editor.getValue().length,
                   sigLen: cards[0].signatureEditor.getValue().length,
                   picker: document.getElementById('card-0-file').value };
        """, "first example loaded into the card")
        print("LOADED", state)
        m.shot(os.path.join(SCRATCH, "s1-loaded.png"))

        # 2. picking another scenario reloads the card (no Load button)
        m.js("""
          const sel = document.getElementById('scenario-select');
          for (const o of sel.options)
            if (o.value !== sel.value) { sel.value = o.value; break; }
          sel.dispatchEvent(new Event('change'));
        """)
        state2 = wait_js(m, f"""
          const card = window.cavocCards[0];
          if (card.name === {json.dumps(state['name'])}) return null;
          if (card.editor.getValue().length === 0) return null;
          return {{ name: card.name, codeLen: card.editor.getValue().length,
                    picker: document.getElementById('card-0-file').value }};
        """, "picker change loads the new file")
        print("PICKED", state2)

        # 3. single mode offers one example selector: the toolbar picker
        #    drives the card, whose own picker is hidden compose-mode chrome
        m.js(f"""
          const sel = document.getElementById('scenario-select');
          sel.value = {json.dumps(state['first'])};
          sel.dispatchEvent(new Event('change'));
        """)
        state3 = wait_js(m, f"""
          const card = window.cavocCards[0];
          if (card.name !== {json.dumps(state['name'])}) return null;
          if (card.editor.getValue().length === 0) return null;
          return {{ name: card.name,
                    cardPickerHidden: getComputedStyle(
                      document.getElementById('card-0-file')).display === 'none' }};
        """, "toolbar picker drives the single card")
        print("ONE-PICKER", state3)

        # 3b. the pane splitter trades height between module and signature;
        #     double-click restores the default split
        resize = m.js("""
          const splitter = document.getElementById('card-0-pane-splitter');
          const modulePane = document.querySelector('#card-0 .card-pane');
          const rect = document.querySelector('#card-0 .card-editors').getBoundingClientRect();
          const before = modulePane.getBoundingClientRect().height;
          const at = (y) => ({ bubbles: true, clientY: y });
          splitter.dispatchEvent(new MouseEvent('mousedown', at(rect.top + rect.height * 0.6)));
          window.dispatchEvent(new MouseEvent('mousemove', at(rect.top + rect.height * 0.85)));
          window.dispatchEvent(new MouseEvent('mouseup', at(rect.top + rect.height * 0.85)));
          const after = modulePane.getBoundingClientRect().height;
          splitter.dispatchEvent(new MouseEvent('dblclick', { bubbles: true }));
          const reset = modulePane.getBoundingClientRect().height;
          return { grew: after > before + 20,
                   resetBack: Math.abs(reset - before) < 2,
                   before: Math.round(before), after: Math.round(after) };
        """)
        print("PANE-RESIZE", resize)

        # 4. Start begins an interaction; moves are clickable rows; the card's
        #    live panels light up with summary strips; the focused first move
        #    peeks its subject through the collapsed ienv panel
        m.js("document.getElementById('submit').click();")
        rows = wait_js(m, """
          const rows = document.querySelectorAll('#moves-list .choice-row');
          if (rows.length === 0) return null;
          return Array.from(rows).map(r => r.textContent);
        """, "choice rows after Start")
        print("MOVES", rows)
        panels = wait_js(m, """
          const ienv = document.getElementById('card-0-ienv-panel');
          const store = document.getElementById('card-0-store-panel');
          if (!ienv.classList.contains('has-data')) return null;
          if (!store.classList.contains('has-data')) return null;
          return { ienvStrip: document.getElementById('card-0-ienv-strip').textContent,
                   storeStrip: document.getElementById('card-0-store-strip').textContent,
                   ienvUnfolded: ienv.classList.contains('state-full'),
                   storeFolded: store.classList.contains('state-collapsed'),
                   peeked: document.querySelectorAll('#card-0-ienv .ienv-item.peek').length,
                   storePeeked: document.querySelectorAll('#card-0-store .peek').length };
        """, "live panels fed after Start")
        print("PANELS", panels)
        running = m.js("""
          return { stopEnabled: !document.getElementById('stop-btn').disabled,
                   startDisabled: document.getElementById('submit').disabled,
                   caption: document.getElementById('choice-caption').textContent };
        """)
        print("RUNNING", running)
        m.shot(os.path.join(SCRATCH, "s2-moves.png"))

        # 4b. once focus has wandered off, clicking the panel chrome (not a
        #     row) hands the keyboard back to the last-focused row
        refocus = m.js("""
          const rows = document.querySelectorAll('#moves-list .choice-row');
          rows[rows.length - 1].focus();
          document.activeElement.blur();
          const wandered = !document.activeElement.classList.contains('choice-row');
          document.getElementById('choice-caption').click();
          return { wandered,
                   refocused: document.activeElement.classList.contains('choice-row'),
                   remembered: document.activeElement === rows[rows.length - 1] };
        """)
        print("REFOCUS", refocus)

        # 5. the strip cycles full -> collapsed -> Δ -> full, and in the full
        #    state an entry expands from its one-line summary
        cycle = m.js("""
          const panel = document.getElementById('card-0-ienv-panel');
          const strip = document.getElementById('card-0-ienv-strip');
          const states = [];
          for (let i = 0; i < 3; i++) {
            strip.click();
            states.push(panel.className.match(/state-[a-z]+/)[0]);
          }
          const entry = document.querySelector('#card-0-ienv .live-entry');
          entry.querySelector('summary').click();
          const opened = { entryOpen: entry.open,
                           previewHidden: getComputedStyle(
                             entry.querySelector('.entry-preview')).display === 'none',
                           fullText: entry.querySelector('.entry-code').textContent.length > 0 };
          entry.querySelector('summary').click();
          return { states, ...opened,
                   entries: document.querySelectorAll('#card-0-ienv .live-entry').length,
                   fresh: document.querySelectorAll('#card-0-ienv .live-entry.fresh').length };
        """)
        print("CYCLE", cycle)

        # 6. clicking a row plays it: a chip lands on the public history lane,
        #    visible in its own window without any tab switching, and the
        #    strips update
        m.js("document.querySelector('#moves-list .choice-row').click();")
        histo = wait_js(m, """
          const lane = document.getElementById('trace-lane-public-chips');
          if (!lane || lane.children.length === 0) return null;
          const visible = (id) =>
            document.getElementById(id).getBoundingClientRect().height > 0;
          return { chips: Array.from(lane.children).map(c => c.textContent),
                   historyVisible: visible('history-window'),
                   sideTabsVisible: visible('side-tabs') };
        """, "history chip after playing a move")
        print("HISTORY", histo)
        strips = m.js("""
          return { ienvStrip: document.getElementById('card-0-ienv-strip').textContent,
                   storeStrip: document.getElementById('card-0-store-strip').textContent };
        """)
        print("STRIPS", strips)

        # 6b. the side tabs rest folded to their buttons; a click unfolds the
        #     pane, its seam trades width with the History window, a second
        #     click on the open tab folds both away
        fold = m.js("""
          const pane = document.getElementById('side-tabs');
          const seam = document.getElementById('lower-splitter');
          const wasFolded = pane.classList.contains('folded');
          const seamHiddenFolded = getComputedStyle(seam).display === 'none';
          const widthFolded = pane.getBoundingClientRect().width;
          document.querySelector('.tab-link[data-tab="config"]').click();
          const unfolded = !pane.classList.contains('folded')
            && document.getElementById('config').classList.contains('active');
          const widthOpen = pane.getBoundingClientRect().width;
          const seamShownOpen = getComputedStyle(seam).display !== 'none';
          const row = document.getElementById('lower-row').getBoundingClientRect();
          const at = (x) => ({ bubbles: true, clientX: x });
          seam.dispatchEvent(new MouseEvent('mousedown', at(row.right - widthOpen)));
          window.dispatchEvent(new MouseEvent('mousemove', at(row.right - widthOpen - 150)));
          window.dispatchEvent(new MouseEvent('mouseup', at(row.right - widthOpen - 150)));
          const widthDragged = pane.getBoundingClientRect().width;
          document.querySelector('.tab-link[data-tab="config"]').click();
          return { wasFolded, seamHiddenFolded, unfolded, seamShownOpen,
                   widened: widthOpen > widthFolded + 40,
                   dragWidened: widthDragged > widthOpen + 100,
                   refolded: pane.classList.contains('folded') };
        """)
        print("SIDE-TABS", fold)
        m.shot(os.path.join(SCRATCH, "s3-played.png"))

        # 7. per-card collapse hides the editors and keeps the live panels
        collapsed = m.js("""
          document.getElementById('card-0-collapse').click();
          const card = document.getElementById('card-0');
          const editors = card.querySelector('.card-editors');
          const r = { collapsed: card.classList.contains('collapsed'),
                      editorsHidden: getComputedStyle(editors).display === 'none',
                      stripVisible: getComputedStyle(
                        document.getElementById('card-0-ienv-strip')).display !== 'none' };
          document.getElementById('card-0-collapse').click();
          return r;
        """)
        print("COLLAPSE", collapsed)

        # 8. Stop interrupts: Stop disabled again, Start back
        m.js("document.getElementById('stop-btn').click();")
        idle = wait_js(m, """
          const stop = document.getElementById('stop-btn');
          const start = document.getElementById('submit');
          if (!stop.disabled || start.disabled) return null;
          return { stopped: true };
        """, "page idle after Stop")
        print("STOPPED", idle)

        # 9. Guide opens with help.md
        m.js("document.getElementById('guide-btn').click();")
        guide = wait_js(m, """
          const p = document.getElementById('guide-panel');
          if (!p.classList.contains('open')) return null;
          const t = document.getElementById('guide-content').textContent;
          if (t.length < 20) return null;
          return t.slice(0, 60);
        """, "guide panel open with content")
        print("GUIDE", json.dumps(guide))
        m.shot(os.path.join(SCRATCH, "s4-guide.png"))
        m.js("document.getElementById('guide-btn').click();")

        # 10. Tutorial enters level 1: options and file pickers hidden, level
        #     badge, auto-start
        m.js("document.getElementById('tutorial-btn').click();")
        tuto = wait_js(m, """
          const ind = document.getElementById('level-indicator');
          if (ind.style.display === 'none') return null;
          const card = (window.cavocCards || [])[0];
          if (!card || !card.name.startsWith('tuto')) return null;
          const rows = document.querySelectorAll('#moves-list .choice-row');
          if (rows.length === 0) return null;
          return { level: ind.textContent, name: card.name,
                   optionsHidden: document.querySelector('.options-dropdown-container').style.display === 'none',
                   filePickerHidden: getComputedStyle(
                     document.getElementById('card-0-file')).display === 'none',
                   guideOpen: document.getElementById('guide-panel').classList.contains('open'),
                   moves: rows.length };
        """, "tutorial level 1 running")
        print("TUTORIAL", tuto)
        m.shot(os.path.join(SCRATCH, "s5-tutorial.png"))

        # 11. exit tutorial: picker back, sandbox reloaded
        m.js("document.getElementById('tutorial-btn').click();")
        back = wait_js(m, """
          const card = (window.cavocCards || [])[0];
          if (!card || card.name.startsWith('tuto')) return null;
          if (document.getElementById('scenario-select').style.display === 'none') return null;
          return { name: card.name };
        """, "back to sandbox")
        print("EXITED", back)

        # 12. compose: picking the composition loads two cards with the
        #     shared gutter between them, the shared History lane, and
        #     Symbolic greyed out; the per-card pickers show
        m.js("""
          const sel = document.getElementById('scenario-select');
          sel.value = 'compose:0';
          sel.dispatchEvent(new Event('change'));
        """)
        compose = wait_js(m, """
          const cards = window.cavocCards || [];
          if (cards.length !== 2) return null;
          if (cards[0].editor.getValue().length === 0) return null;
          if (cards[1].editor.getValue().length === 0) return null;
          return { names: cards.map(c => c.name),
                   roles: Array.from(document.querySelectorAll('.role-badge'))
                            .map(b => b.textContent),
                   gutter: !!document.getElementById('shared-gutter'),
                   traceGrid: !!document.getElementById('trace-grid'),
                   symbolicDisabled: document.getElementById('symbolic-check').disabled,
                   bodyMode: document.body.classList.contains('mode-compose'),
                   pickersShown: getComputedStyle(
                     document.getElementById('card-0-file')).display !== 'none' };
        """, "compose scenario loaded into two cards")
        print("COMPOSE", compose)
        m.shot(os.path.join(SCRATCH, "s6-compose.png"))

        # 13. Start on the composition: the choice panel offers the client's
        #     public name at the composite typing (g, never the provider's f);
        #     the position readout and the seeded span table have data
        m.js("document.getElementById('submit').click();")
        crows = wait_js(m, """
          const rows = document.querySelectorAll('#moves-list .choice-row');
          if (rows.length === 0) return null;
          return Array.from(rows).map(r => r.textContent);
        """, "composite choice rows after Start")
        print("COMPOSE-MOVES", crows)
        gutters = m.js("""
          return { head: document.getElementById('gutter-head').textContent,
                   headVisible: getComputedStyle(
                     document.getElementById('gutter-head')).display !== 'none',
                   spans: document.getElementById('shared-gutter-spans')
                            .textContent.trim() };
        """)
        print("COMPOSE-GUTTERS", gutters)

        # 14. playing g crosses the shared interface: τ chips land on the
        #     shared row of the trace grid, in the columns between the
        #     question and the answer of the public row (the one shared
        #     chronology), and the span table grows with the callbacks'
        #     continuations
        m.js("document.querySelector('#moves-list .choice-row').click();")
        chatter = wait_js(m, """
          const grid = document.getElementById('trace-grid');
          if (!grid) return null;
          const taus = Array.from(grid.querySelectorAll('.trace-tau'));
          const pubs = Array.from(grid.querySelectorAll('.trace-o, .trace-p'));
          if (taus.length < 2 || pubs.length < 2) return null;
          const col = (c) => Number(c.style.gridColumn);
          return { public: pubs.map(c => c.textContent),
                   shared: taus.map(c => c.textContent),
                   interleaved: Math.min(...taus.map(col)) > Math.min(...pubs.map(col))
                             && Math.max(...taus.map(col)) < Math.max(...pubs.map(col)) };
        """, "chattering chips after playing g", timeout=40)
        print("CHATTER", chatter)
        after = m.js("""
          return { spans: document.getElementById('shared-gutter-spans')
                            .textContent.trim(),
                   head: document.getElementById('gutter-head').textContent,
                   ienv0: document.getElementById('card-0-ienv-strip').textContent,
                   ienv1: document.getElementById('card-1-ienv-strip').textContent };
        """)
        print("COMPOSE-AFTER", after)
        m.shot(os.path.join(SCRATCH, "s7-compose-played.png"))

        # 14b. step mode: the mode pair highlights the choice, the slider
        #      yields to the next button, and no synchronization appears
        #      until next is pressed — then each press reveals one
        m.js("document.getElementById('chatter-step-mode-btn').click();")
        m.js("document.querySelectorAll('#moves-list .choice-row')[1].click();")
        time.sleep(1.2)
        paused = m.js("""
          return { shared: document.querySelectorAll('#trace-grid .trace-tau').length,
                   stepActive: document.getElementById('chatter-step-mode-btn').classList.contains('active'),
                   autoActive: document.getElementById('chatter-auto-btn').classList.contains('active'),
                   nextShown: getComputedStyle(document.getElementById('chatter-step-btn')).display !== 'none',
                   sliderHidden: getComputedStyle(document.getElementById('chatter-delay')).display === 'none',
                   caption: document.getElementById('choice-caption').textContent };
        """)
        print("STEP-PAUSED", paused)
        if paused['shared'] != 4:
            raise SystemExit("step mode leaked a synchronization")
        for _ in range(4):
            m.js("document.getElementById('chatter-step-btn').click();")
            time.sleep(0.4)
        stepped = wait_js(m, """
          const sh = document.querySelectorAll('#trace-grid .trace-tau').length;
          const pub = document.querySelectorAll('#trace-grid .trace-o, #trace-grid .trace-p').length;
          if (sh < 8 || pub < 4) return null;
          return { shared: sh, public: pub };
        """, "next clicks release the synchronizations")
        print("STEPPED", stepped)
        m.js("document.getElementById('chatter-auto-btn').click();")

        m.js("document.getElementById('stop-btn').click();")
        wait_js(m, """
          return document.getElementById('stop-btn').disabled ? { ok: true } : null;
        """, "page idle after stopping the composition")

        # 15. leaving compose: a single scenario brings back the one-card
        #     page — no shared gutter, the flat lane back in place of the
        #     trace grid, Symbolic re-enabled
        m.js(f"""
          const sel = document.getElementById('scenario-select');
          sel.value = {json.dumps(state['first'])};
          sel.dispatchEvent(new Event('change'));
        """)
        single = wait_js(m, """
          const cards = window.cavocCards || [];
          if (cards.length !== 1) return null;
          return { gutterGone: !document.getElementById('shared-gutter'),
                   gridGone: !document.getElementById('trace-grid'),
                   laneShown: getComputedStyle(
                     document.getElementById('trace-lane-public')).display !== 'none',
                   symbolicEnabled: !document.getElementById('symbolic-check').disabled,
                   composeClassGone: !document.body.classList.contains('mode-compose') };
        """, "back to a single scenario")
        print("SINGLE-AGAIN", single)

        # 16. console tab content (errors would land there and pull it forward)
        console = m.js("return document.getElementById('console').textContent;")
        print("CONSOLE", json.dumps(console[-400:] if console else ""))
        print("ALL_OK, screenshots in", SCRATCH)
    finally:
        ff.terminate()

if __name__ == "__main__":
    main()
