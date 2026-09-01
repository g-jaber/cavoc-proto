/* ==========================================================================
   Shared page skeleton
   ==========================================================================
   The workspace of index.html: the toolbar, the row of participant cards
   with the public gutter at its right edge, the splitter, the lower row —
   the History window under the cards, the Configuration/Console tabs under
   the choice panel — and the guide panel. The markup is built here; the
   scenario/tutorial logic driving it lives in front/scenario.js.

   The element ids below are read from OCaml (see bin/page_init.ml,
   bin/moves_display.ml and bin/display_config.ml): renaming one here means
   renaming it there too.
   ========================================================================== */

/* Inserts markup exactly where the calling <script> sits, so that the document
   order of the page is the one you read in the HTML file. */
function cavocMount(html) {
    document.currentScript.insertAdjacentHTML('beforebegin', html);
}

/* The toolbar is the one home for global actions: scenario picker, Start and
   Stop, the Options menu, the guide toggle and the server badge. The options
   checkboxes are the menu's UI; their state of record is
   cavocScenario.options (see front/scenario.js), which bin/lts_config.ml
   reads.

   The menu holds two kinds of setting, and the split is #options-lts: those
   inside it select which LTS the next evaluation explores, so bin/page_init.ml
   greys the section out while one runs; those outside act on the interaction
   already running — the synchronization pacing, read afresh at every exchange
   by bin/compose_driver.ml, and the debug log. */
function cavocToolbarMarkup() {
    return `
    <header id="toolbar">
      <span id="wordmark">CAVOC</span>
      <label id="scenario-label" for="scenario-select">scenario:</label>
      <select id="scenario-select" class="tsel"></select>
      <span id="level-indicator" style="display: none;"></span>
      <button id="submit" class="tbtn tbtn-start">Start</button>
      <button id="stop-btn" class="tbtn">Stop</button>
      <div class="options-dropdown-container">
        <button id="options-btn" class="tbtn">Options &#9662;</button>
        <div id="options-menu" class="options-menu-content">
          <div id="options-lts">
            <div class="option-item">
              <input type="checkbox" id="direct-style-check">
              <label for="direct-style-check">Direct Style</label>
            </div>
            <hr class="option-rule">
            <div class="option-item">
              <input type="checkbox" id="wellbracketing-check">
              <label for="wellbracketing-check">WellBracketing</label>
            </div>
            <div class="option-item">
              <input type="checkbox" id="visibility-check">
              <label for="visibility-check">Visibility</label>
            </div>
            <hr class="option-rule">
            <div class="option-item">
              <input type="checkbox" id="symbolic-check">
              <label for="symbolic-check">Symbolic</label>
            </div>
          </div>
          <div id="options-chatter">
            <hr class="option-rule">
            <div class="option-section-label">synchronization</div>
            <div class="chatter-modes">
              <button id="chatter-auto-btn"
                      title="Play the synchronizations at the slider's pace">auto</button>
              <button id="chatter-step-mode-btn"
                      title="Pause before every synchronization">step</button>
            </div>
            <input type="range" id="chatter-delay" min="0" max="2000" step="100"
                   title="Delay between synchronizations (ms)">
          </div>
          <hr class="option-rule">
          <div class="option-item">
            <input type="checkbox" id="debug-log-check">
            <label for="debug-log-check">Debug Log</label>
          </div>
        </div>
      </div>
      <span class="toolbar-spacer"></span>
      <button id="tutorial-btn" class="tbtn">Tutorial</button>
      <button id="guide-btn" class="tbtn">Guide</button>
      <button id="shutdown-btn" class="server-badge" style="display:none"
              title="Stop the local server">
        <span class="server-dot"></span>local server &mdash; stop</button>
    </header>`;
}

/* The workspace: the card row — one participant card per module, built by
   cavocBuildCards below — with the public gutter at its right edge, then the
   splitter and the lower row: the trace always on show in the History
   window below the cards, and the Configuration/Console tabs below the
   choice panel. The public gutter is where the Opponent (the user) lives:
   it hosts the choice panel, and its head is the one home for the live
   readouts of the interfaces a composite has — the outer position, and the
   span of the shared names, a slot each for bin/display_config.ml.
   (bin/compose_driver.ml). */
function cavocWorkspaceMarkup() {
    return `
    <div id="workspace-row">
      <div id="card-row"></div>
      <aside id="public-gutter">
        <div id="gutter-head">
          <div id="gutter-position"></div>
          <div id="gutter-shared"></div>
        </div>
        <div id="choice-panel">
          <div id="choice-caption">Your move</div>
          <div id="moves-list">
            <div class="panel-empty">Pick a scenario and press Start to see the available moves.</div>
          </div>
          <div class="kbd-hint">&#8593;&#8595; browse &middot; Enter plays &middot; Esc stops</div>
        </div>
      </aside>
    </div>
    <div id="splitter" title="Drag to resize the cards"></div>
    <div id="lower-row">
      <div id="history-window">
        <div id="history-head">history</div>
        <div id="history">
          <div class="trace-lane" id="trace-lane-public">
            <span class="trace-lane-label">public</span>
            <div class="trace-lane-chips" id="trace-lane-public-chips"></div>
          </div>
        </div>
      </div>
      <div id="lower-splitter" title="Drag to resize the side pane"></div>
      <aside id="side-tabs" class="folded">
        <div class="tabs">
          <div class="tab-nav">
            <button class="tab-link" data-tab="config" onclick="openTab(event, 'config')">Configuration</button>
            <button class="tab-link" data-tab="console" onclick="openTab(event, 'console')">Console</button>
          </div>
          <div id="config" class="tab-content"></div>
          <div id="console" class="tab-content"></div>
        </div>
      </aside>
    </div>
    <aside id="guide-panel">
      <div id="guide-head">
        <span id="guide-title">Guide</span>
        <button id="guide-close" title="Close the guide">&times;</button>
      </div>
      <div id="guide-content">Loading&hellip;</div>
    </aside>`;
}

/* --------------------------------------------------------------------------
   Participant cards.

   One card per module: a header (name, role badge, file picker, collapse
   toggle) above the code and signature editors, stacked, then the card's
   live panels (IEnv, Store) that bin/display_config.ml feeds during a run.
   window.cavocCards is the per-participant editor registry: OCaml reads the
   sources through it (bin/editor_manager.ml), and the live panels are
   addressed by card index (card-0-ienv, …) so the rendering routes to the
   card its half of the configuration belongs to.
   -------------------------------------------------------------------------- */

window.cavocCards = [];

/* The role a card's signature plays, stated in its chrome: the rightmost
   module's signature is the public interface the user plays on; the modules
   before it (compose mode) share theirs with the next participant. */
function cavocCardRole(mode, index, count) {
    if (index === count - 1) return 'public signature';
    return 'shared interface — not public';
}

function cavocCardMarkup(participant, index, role) {
    const i = index;
    return `
    <section class="participant-card" id="card-${i}">
      <header class="card-head">
        <span class="card-name" id="card-${i}-name"></span>
        <span class="role-badge">${role}</span>
        <span class="card-head-spacer"></span>
        <select class="card-file tsel" id="card-${i}-file" title="Load another module into this card"></select>
        <button class="card-collapse" id="card-${i}-collapse" title="Collapse the editors">&#9662;</button>
      </header>
      <div class="card-editors">
        <div class="card-pane">
          <div class="pane-label">module</div>
          <div class="card-editor" id="card-${i}-code"></div>
        </div>
        <div class="pane-splitter" id="card-${i}-pane-splitter"
             title="Drag to resize the module against the signature — double-click to reset"></div>
        <div class="card-pane card-pane-sig">
          <div class="pane-label">signature</div>
          <div class="card-editor" id="card-${i}-sig"></div>
        </div>
      </div>
      <div class="live-panel state-full" id="card-${i}-ienv-panel">
        <button class="live-strip" id="card-${i}-ienv-strip">interactive environment</button>
        <div class="live-entries" id="card-${i}-ienv"></div>
      </div>
      <div class="live-panel state-collapsed" id="card-${i}-store-panel">
        <button class="live-strip" id="card-${i}-store-strip">store</button>
        <div class="live-entries" id="card-${i}-store"></div>
      </div>
    </section>`;
}

function cavocMountCardEditor(id) {
    const ed = ace.edit(id);
    ed.setTheme("ace/theme/monokai");
    ed.session.setMode("ace/mode/ocaml");
    return ed;
}

/* --------------------------------------------------------------------------
   The chattering control (compose mode): how the composite's internal
   synchronizations play. Its two settings live in the Options menu, its next
   button in the choice panel; both are static markup, so this is wired once
   at page init (front/scenario.js) rather than per card build.
   -------------------------------------------------------------------------- */

/* bin/compose_driver.ml reads this afresh at every exchange: auto plays the
   synchronizations at delayMs pace, step waits for the next button. This
   object is the state of record; the widgets are only its UI. */
window.cavocChattering = { mode: 'auto', delayMs: 400 };

function cavocWireChatterControl() {
    const autoBtn = document.getElementById('chatter-auto-btn');
    if (!autoBtn) return;
    const stepModeBtn = document.getElementById('chatter-step-mode-btn');
    const delay = document.getElementById('chatter-delay');
    /* The two mode buttons are a segmented pair (the active one is
       highlighted, clicking selects); the control the chosen mode uses — the
       pace slider in auto, the next button in step — is shown by the mode
       class on the body, since the two now sit in different panels. */
    const apply = () => {
        const stepping = window.cavocChattering.mode === 'step';
        autoBtn.classList.toggle('active', !stepping);
        stepModeBtn.classList.toggle('active', stepping);
        document.body.classList.toggle('chatter-step', stepping);
    };
    delay.value = window.cavocChattering.delayMs;
    autoBtn.addEventListener('click', () => {
        window.cavocChattering.mode = 'auto';
        apply();
    });
    stepModeBtn.addEventListener('click', () => {
        window.cavocChattering.mode = 'step';
        apply();
    });
    delay.addEventListener('input', () => {
        window.cavocChattering.delayMs = Number(delay.value);
    });
    apply();
}

/* The single-module page keeps its flat public lane; a compose scenario
   swaps in the trace grid — the one chronology across both interfaces,
   one row per interface and one column per move, so the τ moves sit
   between the two external moves they happened between. The grid is
   filled by bin/moves_manager.ml; CSS hides the flat lane behind it. */
function cavocSetHistoryLanes(mode) {
    const history = document.getElementById('history');
    const grid = document.getElementById('trace-grid');
    if (mode === 'compose' && !grid) {
        history.insertAdjacentHTML('beforeend',
            '<div class="trace-grid" id="trace-grid"></div>');
    } else if (mode !== 'compose' && grid) {
        grid.remove();
    }
}

/* A live panel is a summary strip over its entries; clicking the strip
   cycles it through its three states — collapsed (strip only, the focused
   move's subject still peeks through), the Δ of the last move, the full
   state. The interactive environment rests unfolded (state-full): it is the
   list of values the user may play with. The store rests collapsed. The
   state classes live on the panel node, which survives re-renders:
   bin/display_config.ml only replaces the strip text and the entries. */
const cavocPanelStates = ['state-collapsed', 'state-delta', 'state-full'];

function cavocCyclePanelState(panel) {
    const current = cavocPanelStates.findIndex((c) => panel.classList.contains(c));
    panel.classList.remove(cavocPanelStates[current]);
    panel.classList.add(cavocPanelStates[(current + 1) % cavocPanelStates.length]);
}

function cavocWireCard(index) {
    const card = document.getElementById(`card-${index}`);
    card.querySelectorAll('.live-strip').forEach((strip) => {
        strip.addEventListener('click', () =>
            cavocCyclePanelState(strip.parentElement));
    });
    const paneSplitter = document.getElementById(`card-${index}-pane-splitter`);
    paneSplitter.addEventListener('mousedown', (e) => {
        cavocPaneDrag = {
            editors: card.querySelector('.card-editors'),
            modulePane: card.querySelector('.card-pane'),
            signaturePane: card.querySelector('.card-pane-sig'),
        };
        document.body.classList.add('resizing');
        e.preventDefault();
    });
    paneSplitter.addEventListener('dblclick', () => {
        card.querySelector('.card-pane').style.flexGrow = '';
        card.querySelector('.card-pane-sig').style.flexGrow = '';
        cavocResizeEditors();
    });
    const collapse = document.getElementById(`card-${index}-collapse`);
    collapse.addEventListener('click', () => {
        const collapsed = card.classList.toggle('collapsed');
        collapse.innerHTML = collapsed ? '&#9656;' : '&#9662;';
        collapse.title =
            collapsed ? 'Expand the editors' : 'Collapse the editors';
        if (!collapsed) cavocResizeEditors();
    });
}

/* Rebuilds the card row for the given participants, releasing the previous
   cards' editors first. The file pickers are left empty: front/scenario.js
   populates and wires them, since it owns the example list and the loading. */
function cavocBuildCards(participants, mode) {
    window.cavocCards.forEach((card) => {
        card.editor.destroy();
        card.signatureEditor.destroy();
    });
    const row = document.getElementById('card-row');
    /* In single mode the toolbar's scenario picker is the one example
       selector, so the card's own picker would be a duplicate: front/style.css
       hides it on this class. Compose scenarios show it, one per card. */
    row.classList.remove('mode-single', 'mode-compose');
    row.classList.add('mode-' + mode);
    /* The mode also scopes chrome outside the row: the shared History lane
       and the public gutter's position readout are compose-mode only. */
    document.body.classList.toggle('mode-compose', mode === 'compose');
    cavocSetHistoryLanes(mode);
    row.innerHTML = participants
        .map((p, i) =>
            cavocCardMarkup(p, i, cavocCardRole(mode, i, participants.length)))
        .join('');
    window.cavocCards = participants.map((p, i) => {
        document.getElementById(`card-${i}-name`).textContent = p.name;
        cavocWireCard(i);
        return {
            name: p.name,
            editor: cavocMountCardEditor(`card-${i}-code`),
            signatureEditor: cavocMountCardEditor(`card-${i}-sig`),
        };
    });
}

function cavocResizeEditors() {
    window.cavocCards.forEach((card) => {
        card.editor.resize();
        card.signatureEditor.resize();
    });
    if (window.configEditor) window.configEditor.resize();
}

/* The pane splitter drag in progress, if any: set by the mousedown handler of
   a card's splitter (cavocWireCard), consumed by the one-time window handlers
   below. One shared slot rather than per-card window listeners, so rebuilding
   the cards leaks nothing. */
let cavocPaneDrag = null;

/* Lets the three seams be dragged: the splitter between the card row and
   the lower row (the --cards-h grid track); the pane splitter of each card,
   trading flex-grow between the module and signature panes (their flex
   basis is 0, so the ratio is the whole layout); and the lower splitter,
   setting the width of the unfolded side pane (--side-tabs-w, which
   survives folding and unfolding). */
function cavocInitSplitter() {
    const splitter = document.getElementById('splitter');
    if (!splitter) return;
    let dragging = false;
    let lowerDragging = false;
    splitter.addEventListener('mousedown', (e) => {
        dragging = true;
        document.body.classList.add('resizing');
        e.preventDefault();
    });
    const lowerSplitter = document.getElementById('lower-splitter');
    lowerSplitter.addEventListener('mousedown', (e) => {
        lowerDragging = true;
        document.body.classList.add('resizing');
        e.preventDefault();
    });
    window.addEventListener('mousemove', (e) => {
        if (dragging) {
            const style = getComputedStyle(document.body);
            const contentTop = document.body.getBoundingClientRect().top + parseFloat(style.paddingTop);
            const h = Math.max(120, Math.min(window.innerHeight * 0.85, e.clientY - contentTop));
            document.body.style.setProperty('--cards-h', h + 'px');
            cavocResizeEditors();
        } else if (lowerDragging) {
            const lowerRow = document.getElementById('lower-row');
            const rect = lowerRow.getBoundingClientRect();
            const w = Math.max(220,
                Math.min(rect.width * 0.6, rect.right - e.clientX));
            lowerRow.style.setProperty('--side-tabs-w', w + 'px');
            if (window.configEditor) window.configEditor.resize();
        } else if (cavocPaneDrag) {
            const rect = cavocPaneDrag.editors.getBoundingClientRect();
            const fraction = Math.min(0.9,
                Math.max(0.1, (e.clientY - rect.top) / rect.height));
            cavocPaneDrag.modulePane.style.flexGrow = fraction;
            cavocPaneDrag.signaturePane.style.flexGrow = 1 - fraction;
            cavocResizeEditors();
        }
    });
    window.addEventListener('mouseup', () => {
        if (!dragging && !lowerDragging && !cavocPaneDrag) return;
        dragging = false;
        lowerDragging = false;
        cavocPaneDrag = null;
        document.body.classList.remove('resizing');
        cavocResizeEditors();
    });
}

/* The side tabs rest folded to their tab buttons; the History window takes
   the width they free. Opening a tab unfolds the pane, clicking the open
   tab folds it back. */
function cavocFoldSideTabs() {
    document.getElementById('side-tabs').classList.add('folded');
    document.querySelectorAll(".tab-content").forEach(
        (c) => c.classList.remove("active"));
    document.querySelectorAll(".tab-link").forEach(
        (l) => l.classList.remove("active"));
}

/* Brings a side tab forward, unfolding the pane, given the id of its content
   div. The single source of truth for tab switching: openTab (the button
   onclick) and Ui_helpers.show_tab on the OCaml side both go through it —
   so an error surfacing on the Console unfolds it too. */
function cavocShowTab(tabId) {
    document.getElementById('side-tabs').classList.remove('folded');
    document.querySelectorAll(".tab-content").forEach(
        (c) => c.classList.remove("active"));
    document.querySelectorAll(".tab-link").forEach(
        (l) => l.classList.remove("active"));
    const content = document.getElementById(tabId);
    if (content) content.classList.add("active");
    const link = document.querySelector('.tab-link[data-tab="' + tabId + '"]');
    if (link) link.classList.add("active");
    /* The Configuration tab is an ACE editor: it only measures itself
       correctly once visible. */
    if (tabId === 'config' && window.configEditor) window.configEditor.resize();
}

function openTab(event, tabId) {
    const link = document.querySelector('.tab-link[data-tab="' + tabId + '"]');
    if (link && link.classList.contains('active')) cavocFoldSideTabs();
    else cavocShowTab(tabId);
}

/* Must run once the markup above is mounted, and before explore_web.bc.js:
   Display_config reads the configEditor instance through the window object.
   The card editors are mounted per scenario by cavocBuildCards. */
function cavocInitAce() {
    const configEditor = ace.edit("config");
    configEditor.setTheme("ace/theme/monokai");
    configEditor.session.setMode("ace/mode/ocaml");
    configEditor.setReadOnly(true);
    configEditor.renderer.$cursorLayer.element.style.display = "none";
    window.configEditor = configEditor;

    cavocInitSplitter();
}

function openModal(modal) {
    modal.style = 'display: flex';
}

function closeModal(modal) {
    modal.style = 'display: none';
}

function setModalTitle(modal, title) {
    let titleTag = modal.getElementsByClassName('win-title');
    if (titleTag.length === 0) {
        const newTitleTag = document.createElement('h1');
        newTitleTag.classList.add('win-title');
        newTitleTag.innerHTML = title;
        modal.getElementsByClassName('win-modal-content')[0].prepend(newTitleTag);
    }

    else titleTag[0].innerHTML = title;
}

function setModalMessage(modal, message) {
    modal.getElementsByClassName('win-message')[0].innerHTML = message;
}

function addModalButton(modal, text, onClick) {
    const btn = document.createElement('button');

    btn.innerText = text;
    btn.classList.add('win-reset-btn');
    btn.addEventListener('click', onClick);

    const content = modal.getElementsByClassName('win-modal-content')[0];
    content.appendChild(btn);
}

function createModal(id, withCloseButton) {
    const modal   = document.createElement('div');
    const content = document.createElement('div');
    const par     = document.createElement('p');

    modal.id = id;

    modal.classList.add('win-modal');
    content.classList.add('win-modal-content');
    par.classList.add('win-message');

    if(withCloseButton) {
        const close = document.createElement('span');
        close.classList.add('close-btn');
        close.addEventListener('click', (_) => closeModal(modal));
        close.innerHTML = '&times;';
        content.appendChild(close);
    }

    modal.appendChild(content);
    content.appendChild(par);

    modal.style = 'display: none';

    document.getElementsByTagName('body')[0].appendChild(modal);

    return modal;
}
