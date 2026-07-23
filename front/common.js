/* ==========================================================================
   Shared page skeleton
   ==========================================================================
   index.html and indextuto.html present the same workspace: two ACE editors,
   the help/shutdown column, and the interaction panel (moves list, controls
   and the configuration tabs). Only the surrounding chrome differs, so the
   markup is built here once and mounted by both pages.

   The element ids below are read from OCaml (see bin/page_init.ml,
   bin/moves_display.ml and bin/display_config.ml): renaming one here means
   renaming it there too.
   ========================================================================== */

/* Inserts markup exactly where the calling <script> sits, so that the document
   order of the page is the one you read in the HTML file. */
function cavocMount(html) {
    document.currentScript.insertAdjacentHTML('beforebegin', html);
}

function cavocEditorsMarkup({ codeTitle, signatureTitle }) {
    return `
    <div class="container code-section">
      <h2>${codeTitle}</h2>
      <div id="editor"></div>
    </div>

    <div class="container signature">
      <h2>${signatureTitle}</h2>
      <div id="signatureEditor"></div>
    </div>

    <div class="button-column">
      <div id="help" class="help">
        <button id="help-btn" class="help-btn">Help</button>
      </div>
      <div id="shutdown" class="shutdown">
        <button id="shutdown-btn" class="shutdown-btn">Shutdown</button>
      </div>
      <button id="toggle-editors" class="toggle-editors" onclick="cavocToggleEditors(this)">Hide editors</button>
    </div>

    <div id="help-modal" class="modal">
      <div class="modal-content">
        <span class="close-btn">&times;</span>
        <div id="markdown-content">Loading…</div>
      </div>
    </div>`;
}

/* The options dropdown, shown only on the free-use page: the tutorial drives
   these settings itself, from the level's JSON file. */
const CAVOC_OPTIONS_MENU_MARKUP = `
          <div class="options-dropdown-container">
            <button id="options-btn" class="options-btn">Options &#9881;&#65039;</button>
            <div id="options-menu" class="options-menu-content">
              <div class="option-item">
                <input type="checkbox" id="direct-style-check">
                <label for="direct-style-check">Direct Style</label>
              </div>
              <hr style="border: 0; border-top: 1px solid #555; margin: 5px 0;">
              <div class="option-item">
                <input type="checkbox" id="wellbracketing-check">
                <label for="wellbracketing-check">WellBracketing</label>
              </div>
              <div class="option-item">
                <input type="checkbox" id="visibility-check">
                <label for="visibility-check">Visibility</label>
              </div>
              <hr style="border: 0; border-top: 1px solid #555; margin: 5px 0;">
              <div class="option-item">
                <input type="checkbox" id="symbolic-check">
                <label for="symbolic-check">Symbolic</label>
              </div>
              <hr style="border: 0; border-top: 1px solid #555; margin: 5px 0;">
              <div class="option-item">
                <input type="checkbox" id="debug-log-check">
                <label for="debug-log-check">Debug Log</label>
              </div>
            </div>
          </div>`;

/* Navigation between the configurations produced by a symbolic LTS. Optional:
   Evaluate_code.choose_conf falls back to the first configuration when these
   buttons are absent. */
const CAVOC_CONFIG_NAV_MARKUP = `
          <div style="display: flex; flex-direction: column">
            <span>Configurations: </span>
            <div>
              <button id="conf-prev">Previous</button>
              <button id="conf-next">Next</button>
              <button id="conf-accept">Accept</button>
            </div>
          </div>`;

function cavocWorkspaceMarkup({ optionsMenu = false, configNav = false } = {}) {
    return `
    <div id="output-container">
      <div style="flex: 2">
        <div id="buttons-container">
          <button id="select-btn">Select</button>
          <button id="stop-btn">Stop</button>
        </div>
        <div id="select-move">
          <div id="moves-list"></div>
        </div>
      </div>
      <div style="margin-left: 1%; flex: 7;">
        <div id="center-controls">${optionsMenu ? CAVOC_OPTIONS_MENU_MARKUP : ''}
          <button id="submit">Evaluate</button>${configNav ? CAVOC_CONFIG_NAV_MARKUP : ''}
        </div>
        <div id="histo-config-container">
          <div class="tabs">
            <div class="tab-nav">
              <button class="tab-link" data-tab="config" onclick="openTab(event, 'config')">Configuration</button>
              <button class="tab-link active" data-tab="ienv" onclick="openTab(event, 'ienv')">IEnv</button>
              <button class="tab-link" data-tab="store" onclick="openTab(event, 'store')">Store</button>
              <button class="tab-link" data-tab="histo" onclick="openTab(event, 'histo')">History</button>
              <button class="tab-link" data-tab="console" onclick="openTab(event, 'console')">Console</button>
            </div>

            <div id="config" class="tab-content"></div>
            <div id="ienv" class="tab-content active">
              <div id="ienv-div" class="tab-content active"></div>
              <div id="stack-container" class="tab-content active"></div>
            </div>
            <div id="store" class="tab-content"></div>
            <div id="histo" class="tab-content">
              <div id="history"></div>
            </div>
            <div id="console" class="tab-content"></div>
          </div>
        </div>
      </div>
    </div>`;
}

/* Collapses the two code editors to give the interaction area more room. The
   collapsed state is a single class on <body>, so the default layout is exactly
   what it was before this toggle existed. */
function cavocToggleEditors(btn) {
    const collapsed = document.body.classList.toggle('editors-collapsed');
    btn.textContent = collapsed ? 'Show editors' : 'Hide editors';
    if (!collapsed) {
        // ACE renders at zero size while its container is hidden, so it must be
        // resized once the editors are shown again.
        if (window.editor_instance) window.editor_instance.resize();
        if (window.signatureEditor_instance) window.signatureEditor_instance.resize();
    }
}

function openTab(event, tabId) {
    // Hide all tab contents
    const contents = document.querySelectorAll(".tab-content");
    contents.forEach((content) => content.classList.remove("active"));

    // Remove "active" class from all tab links
    const links = document.querySelectorAll(".tab-link");
    links.forEach((link) => link.classList.remove("active"));

    // Show the selected tab content, and mark its link as active
    document.getElementById(tabId).classList.add("active");
    event.currentTarget.classList.add("active");
}

/* Must run once the markup above is mounted, and before explore_web.bc.js:
   Editor_manager reads these instances through the window object. */
function cavocInitAce() {
    const editor = ace.edit("editor");
    editor.setTheme("ace/theme/monokai");
    editor.session.setMode("ace/mode/ocaml");

    const signatureEditor = ace.edit("signatureEditor");
    signatureEditor.setTheme("ace/theme/monokai");
    signatureEditor.session.setMode("ace/mode/ocaml");

    const configEditor = ace.edit("config");
    configEditor.setTheme("ace/theme/monokai");
    configEditor.session.setMode("ace/mode/ocaml");
    configEditor.setReadOnly(true);
    configEditor.renderer.$cursorLayer.element.style.display = "none";

    window.editor_instance = editor;
    window.signatureEditor_instance = signatureEditor;
    window.configEditor_instance = configEditor;

    // Shorter aliases, used by the page-specific scripts.
    window.editor = editor;
    window.signatureEditor = signatureEditor;
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
    // const title   = document.createElement('h2');
    const par     = document.createElement('p');

    modal.id = id;

    modal.classList.add('win-modal');
    content.classList.add('win-modal-content');
    // title.classList.add('win-title');
    par.classList.add('win-message');

    if(withCloseButton) {
        const close = document.createElement('span');
        close.classList.add('close-btn');
        close.addEventListener('click', (_) => closeModal(modal));
        close.innerHTML = '&times;';
        content.appendChild(close);
    }

    modal.appendChild(content);
    // content.appendChild(title);
    content.appendChild(par);

    modal.style = 'display: none';

    document.getElementsByTagName('body')[0].appendChild(modal);

    return modal;
}
