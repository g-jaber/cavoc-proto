/* ==========================================================================
   Scenario descriptor and page-mode driver
   ==========================================================================
   A scenario is the one format behind the examples dropdown and the tutorial
   levels (doc/web.md): the mode, the participants (their files), the LTS
   options and an optional guide text.

     { title, mode: 'single',
       dir,            // the test/ directory it was loaded from, if any
       participants: [ { name, dir, ml, mli } ],
       options: { directStyle, wellBracketing, visibility, innocence,
                  symbolic },
       guide,          // markdown URL, or null
       locked }        // tutorial levels lock the options

   Every example lives in its own test/<dir>/ holding the sources and a
   scenario.json in the same shape but with directory-relative file names;
   test/manifest.json lists the directories (and the tutorial's ordered
   selection of them), so the page needs no server-side directory listing
   and an example is addressable as #<dir> in the URL.

   window.cavocScenario is the state of record for the options:
   bin/lts_config.ml reads it, the Options menu edits it in sandbox mode, and
   the tutorial levels overwrite it. The checkboxes of the menu are only its
   UI. The participants materialize as the row of cards (cavocBuildCards,
   front/common.js), whose editors bin/editor_manager.ml reads.

   The tutorial is the page driven by a scripted sequence of scenarios
   (tuto/tutoN.*); the sandbox is the same page with an editable one.
   ========================================================================== */

window.cavocScenario = {
    title: 'scratch',
    mode: 'single',
    participants: [{ name: 'scratch', ml: null, mli: null }],
    options: { directStyle: false, wellBracketing: false,
               visibility: false, innocence: false, symbolic: false },
    guide: 'help.md',
    locked: false,
};

/* null in sandbox mode; { level } while the tutorial script drives the page. */
let cavocTutorial = null;
const cavocLevelKey = 'cavoc-current-level';

/* The parsed test/manifest.json: { examples: [ { dir, title, mode } ],
   tutorial: [ dir ] }, fetched once at startup. null when unreachable. */
let cavocManifest = null;

const cavocOptionIds = {
    directStyle: 'direct-style-check',
    wellBracketing: 'wellbracketing-check',
    visibility: 'visibility-check',
    innocence: 'innocence-check',
    symbolic: 'symbolic-check',
};

/* --- options: object <-> menu ------------------------------------------- */

function cavocApplyOptionsToMenu() {
    const options = window.cavocScenario.options;
    for (const key in cavocOptionIds) {
        const box = document.getElementById(cavocOptionIds[key]);
        if (box) box.checked = !!options[key];
    }
    cavocEnforceDirectStyleLock();
    cavocEnforceComposeLock();
    cavocEnforceSynthesisLock();
}

/* The synthesis page synthesizes both participants from the play, so every
   option is forced: the concrete CPS stack, and the two restrictions that
   keep the play definable (Lts_kind.build_arena forces them too). */
function cavocEnforceSynthesisLock() {
    const forced = { directStyle: false, symbolic: false,
                     visibility: true, wellBracketing: true };
    if (window.cavocScenario.mode !== 'synthesis') {
        /* The other locks own every box but Visibility. */
        const visibility = document.getElementById(cavocOptionIds.visibility);
        if (visibility) visibility.disabled = false;
        return;
    }
    for (const key in forced) {
        window.cavocScenario.options[key] = forced[key];
        const box = document.getElementById(cavocOptionIds[key]);
        if (!box) continue;
        box.checked = forced[key];
        box.disabled = true;
    }
}

/* Two options are greyed out in compose mode: Symbolic, because the
   composition is built on the concrete stack (Lts_kind.build_compose_lts is
   single-result only), and Direct Style, because forwarding moves across the
   shared interface needs the CPS representation of actions (the same
   incompatibility explore_cli states for -no-cps with -compose). */
function cavocEnforceComposeLock() {
    const symbolic = document.getElementById('symbolic-check');
    const direct = document.getElementById('direct-style-check');
    if (!symbolic || !direct) return;
    if (window.cavocScenario.mode === 'compose') {
        window.cavocScenario.options.symbolic = false;
        symbolic.checked = false;
        symbolic.disabled = true;
        window.cavocScenario.options.directStyle = false;
        direct.checked = false;
        direct.disabled = true;
        /* Dropping Direct Style hands WellBracketing back to the user. */
        cavocEnforceDirectStyleLock();
    } else if (window.cavocScenario.mode !== 'synthesis') {
        symbolic.disabled = false;
        direct.disabled = false;
    }
}

/* Direct style is intrinsically well-bracketed, so it implies WellBracketing
   (same rule as explore_cli's -no-cps); Innocence is enforced in CPS only. */
function cavocEnforceDirectStyleLock() {
    const wb = document.getElementById('wellbracketing-check');
    const innocence = document.getElementById('innocence-check');
    if (!wb || !innocence) return;
    if (window.cavocScenario.options.directStyle) {
        window.cavocScenario.options.wellBracketing = true;
        wb.checked = true;
        wb.disabled = true;
        window.cavocScenario.options.innocence = false;
        innocence.checked = false;
        innocence.disabled = true;
    } else {
        innocence.disabled = false;
        if (window.cavocScenario.mode !== 'synthesis') wb.disabled = false;
    }
}

function cavocWireOptionsMenu() {
    const btn = document.getElementById('options-btn');
    const menu = document.getElementById('options-menu');
    btn.addEventListener('click', (e) => {
        e.stopPropagation();
        menu.style.display = menu.style.display === 'block' ? 'none' : 'block';
    });
    document.addEventListener('click', (e) => {
        if (menu.style.display === 'block' && !menu.contains(e.target) && e.target !== btn)
            menu.style.display = 'none';
    });
    for (const key in cavocOptionIds) {
        const box = document.getElementById(cavocOptionIds[key]);
        if (!box) continue;
        box.addEventListener('change', () => {
            window.cavocScenario.options[key] = box.checked;
            cavocEnforceDirectStyleLock();
        });
    }
}

/* --- running state ------------------------------------------------------- */

/* Page_init disables the Stop button while no interaction runs, so its
   enabled state is the page's "running" flag. */
function cavocIsRunning() {
    const stop = document.getElementById('stop-btn');
    return stop && !stop.disabled;
}

/* Changing scenario or file during a run stops the run: the same interrupt
   path as pressing Stop, which the OCaml side is waiting on. */
function cavocStopIfRunning() {
    if (cavocIsRunning()) document.getElementById('stop-btn').click();
}

/* --- loading a scenario -------------------------------------------------- */

async function cavocFetchText(url) {
    const response = await fetch(url);
    if (!response.ok) throw new Error(`${url}: ${response.status}`);
    return response.text();
}

/* Loads a participant's files into its card: the editors, the header name,
   and the registry name that bin/editor_manager.ml turns into the lexer's
   file names. */
async function cavocLoadParticipant(index, participant) {
    const card = window.cavocCards[index];
    card.name = participant.name;
    document.getElementById(`card-${index}-name`).textContent = participant.name;
    let ml = '';
    let mli = '';
    if (participant.ml) {
        try { ml = await cavocFetchText(participant.ml); }
        catch (e) { console.error('Error loading the module:', e); }
    }
    if (participant.mli) {
        /* The signature is optional: a missing .mli leaves the editor empty. */
        try { mli = await cavocFetchText(participant.mli); } catch (e) {}
    }
    card.editor.session.setValue(ml);
    card.signatureEditor.session.setValue(mli);
}

/* Fills a card's file picker with the single-module examples of the
   manifest, and marks the current participant's directory as selected. A
   participant from outside those (a compose component, a scratch card)
   appears as an extra entry for its own name, so the picker always shows
   what the card holds. */
function cavocFillCardPicker(index, participant) {
    const select = document.getElementById(`card-${index}-file`);
    select.innerHTML = '';
    const singles = cavocManifest
        ? cavocManifest.examples.filter((e) => e.mode === 'single')
        : [];
    const listed = singles.some((e) => e.dir === participant.dir);
    if (!listed) {
        /* The extra entry stands for the participant itself; the empty
           value distinguishes it from every directory. */
        const own = document.createElement('option');
        own.value = '';
        own.text = participant.name;
        select.appendChild(own);
    }
    singles.forEach((e) => {
        const option = document.createElement('option');
        option.value = e.dir;
        option.text = e.title;
        select.appendChild(option);
    });
    select.value = listed ? participant.dir : '';
    select.addEventListener('change', async () => {
        cavocStopIfRunning();
        const chosen = select.value === ''
            ? participant
            : (await cavocScenarioFromDir(select.value))?.participants[0];
        if (!chosen) return;
        window.cavocScenario.participants[index] = chosen;
        cavocLoadParticipant(index, chosen);
        /* In single mode the one card and the toolbar picker say the same
           thing, so changing either keeps the other in step. */
        if (window.cavocScenario.mode === 'single' && select.value !== '') {
            window.cavocScenario.title = chosen.name;
            window.cavocScenario.dir = select.value;
            const scenarioSelect = document.getElementById('scenario-select');
            if ([...scenarioSelect.options].some((o) => o.value === select.value))
                scenarioSelect.value = select.value;
            history.replaceState(null, '', `#${select.value}`);
        }
    });
}

async function cavocLoadScenario(scenario) {
    window.cavocScenario = scenario;
    cavocApplyOptionsToMenu();
    cavocBuildCards(scenario.participants, scenario.mode);
    scenario.participants.forEach((p, i) => cavocFillCardPicker(i, p));
    await Promise.all(
        scenario.participants.map((p, i) => cavocLoadParticipant(i, p)));
    if (scenario.guide) cavocLoadGuide(scenario.guide);
}

/* --- the guide panel ----------------------------------------------------- */

function cavocOpenGuide() {
    document.getElementById('guide-panel').classList.add('open');
}

function cavocToggleGuide() {
    document.getElementById('guide-panel').classList.toggle('open');
}

async function cavocLoadGuide(url) {
    const content = document.getElementById('guide-content');
    try {
        content.innerHTML = marked.parse(await cavocFetchText(url));
    } catch (e) {
        content.textContent = 'No guide text available.';
    }
}

/* --- the examples of test/: one directory each ---------------------------- */

function cavocExampleUrl(dir, file) {
    /* Relative URL: the page works whatever host serves it. */
    return `../test/${dir}/${file}`;
}

/* Builds the scenario of test/<dir>/ from its scenario.json, resolving the
   directory-relative file names to URLs. For a compose example the first
   participant provides the shared interface, the second is its client and
   the only one whose signature is public. */
async function cavocScenarioFromDir(dir) {
    let described;
    try {
        const response = await fetch(cavocExampleUrl(dir, 'scenario.json'));
        if (!response.ok) throw new Error(`${response.status}`);
        described = await response.json();
    } catch (e) {
        console.error(`Error loading the scenario of ${dir}:`, e);
        return null;
    }
    const mode = described.mode || 'single';
    const participants = described.participants.map((p) => ({
        name: p.name,
        dir,
        ml: p.ml ? cavocExampleUrl(dir, p.ml) : null,
        /* The signature is optional: a missing .mli leaves the editor
           empty. */
        mli: p.mli ? cavocExampleUrl(dir, p.mli) : null,
    }));
    /* The synthesis page's second card is the synthesized client: a
       participant with no source of its own. */
    if (mode === 'synthesis')
        participants.push({ name: 'client', dir, ml: null, mli: null });
    return {
        title: described.title || dir,
        dir,
        mode,
        participants,
        /* An example without options of its own leaves the menu's current
           choices in force (cavocEnforceComposeLock still turns Symbolic
           and Direct Style off in compose mode). */
        options: described.options
            ? { directStyle: !!described.options.directStyle,
                wellBracketing: !!described.options.wellBracketing,
                visibility: !!described.options.visibility,
                innocence: !!described.options.innocence,
                symbolic: !!described.options.symbolic }
            : window.cavocScenario.options,
        guide: described.guide ? cavocExampleUrl(dir, described.guide) : null,
        locked: !!described.locked,
    };
}

/* The toolbar picker's values are manifest directories, single-module and
   compose alike. Picking one makes it the page's URL fragment, so any
   loaded example is linkable as #<dir>. */
async function cavocLoadPickedScenario(dir) {
    const scenario = await cavocScenarioFromDir(dir);
    if (!scenario) return;
    await cavocLoadScenario(scenario);
    history.replaceState(null, '', `#${dir}`);
}

async function cavocPopulateExamples() {
    try {
        const response = await fetch('../test/manifest.json');
        if (!response.ok) throw new Error(`${response.status}`);
        cavocManifest = await response.json();
    } catch (e) {
        console.log('Error loading the examples manifest:', e);
        cavocManifest = null;
    }
    const select = document.getElementById('scenario-select');
    select.innerHTML = '';
    if (!cavocManifest) return;
    cavocManifest.examples
        .filter((e) => e.mode === 'single')
        .forEach((e) => {
            const option = document.createElement('option');
            option.value = e.dir;
            option.text = e.title;
            select.appendChild(option);
        });
    const addGroup = (label, mode) => {
        const examples = cavocManifest.examples.filter((e) => e.mode === mode);
        if (examples.length === 0) return;
        const group = document.createElement('optgroup');
        group.label = label;
        examples.forEach((e) => {
            const option = document.createElement('option');
            option.value = e.dir;
            option.text = e.title;
            group.appendChild(option);
        });
        select.appendChild(group);
    };
    addGroup('compositions', 'compose');
    addGroup('synthesis', 'synthesis');
}

/* --- tutorial: a scripted sequence of scenarios --------------------------- */

/* Level n is the n-th example of the manifest's tutorial list, its options
   locked whatever its scenario.json says. */
async function cavocTutorialScenario(level) {
    const scenario =
        await cavocScenarioFromDir(cavocManifest.tutorial[level - 1]);
    if (scenario) scenario.locked = true;
    return scenario;
}

async function cavocLoadLevel(level) {
    /* Past the last entry of the manifest's list means the tutorial is
       over (an end condition that needs no 404, which a static host with
       a fallback page would not produce). */
    if (!cavocManifest || level > cavocManifest.tutorial.length) {
        openModal(cavocTutoEndModal);
        return;
    }
    const scenario = await cavocTutorialScenario(level);
    if (!scenario) return;
    cavocTutorial = { level };
    localStorage.setItem(cavocLevelKey, level);
    const indicator = document.getElementById('level-indicator');
    indicator.textContent = `Level ${level}`;
    indicator.style.display = '';
    await cavocLoadScenario(scenario);
    cavocOpenGuide();
    history.replaceState(null, '', `#tuto/${level}`);
    /* A level starts playing at once. */
    document.getElementById('submit').click();
}

/* Entered at the stored level, unless a #tuto/<n> deep link named one. */
function cavocEnterTutorial(level) {
    cavocStopIfRunning();
    document.getElementById('scenario-label').style.display = 'none';
    document.getElementById('scenario-select').style.display = 'none';
    /* The level drives the options and the files, so the menu and the cards'
       file pickers are hidden rather than fought over (body.tutorial also
       covers the cards of later levels). */
    document.querySelector('.options-dropdown-container').style.display = 'none';
    document.body.classList.add('tutorial');
    document.getElementById('tutorial-btn').textContent = 'Exit tutorial';
    const stored = parseInt(localStorage.getItem(cavocLevelKey) || '1', 10);
    cavocLoadLevel(level || (stored > 0 ? stored : 1));
}

function cavocExitTutorial() {
    cavocStopIfRunning();
    cavocTutorial = null;
    document.getElementById('scenario-label').style.display = '';
    document.getElementById('scenario-select').style.display = '';
    document.querySelector('.options-dropdown-container').style.display = '';
    document.body.classList.remove('tutorial');
    document.getElementById('level-indicator').style.display = 'none';
    document.getElementById('tutorial-btn').textContent = 'Tutorial';
    cavocLoadGuide('help.md');
    const select = document.getElementById('scenario-select');
    if (select.value) cavocLoadPickedScenario(select.value);
    else history.replaceState(null, '', location.pathname + location.search);
}

/* --- outcome modals ------------------------------------------------------ */

let cavocEndedModal = null;
let cavocTutoSuccessModal = null;
let cavocTutoEndModal = null;

/* Called by OCaml (see bin/evaluate_code.ml) once the interaction is over. */
function onSuccess(message) {
    if (cavocTutorial) {
        setModalMessage(cavocTutoSuccessModal, message);
        openModal(cavocTutoSuccessModal);
    } else {
        setModalMessage(cavocEndedModal, message);
        openModal(cavocEndedModal);
    }
}

function cavocBuildModals() {
    cavocEndedModal = createModal('interaction-ended', true);
    setModalTitle(cavocEndedModal, 'Interaction ended');
    addModalButton(cavocEndedModal, 'Ok', (_) => closeModal(cavocEndedModal));

    cavocTutoSuccessModal = createModal('tuto-success', false);
    setModalTitle(cavocTutoSuccessModal, '\u{1F3C6} SUCCESS');
    addModalButton(cavocTutoSuccessModal, 'Next level >>', (_) => {
        closeModal(cavocTutoSuccessModal);
        cavocLoadLevel(cavocTutorial.level + 1);
    });

    cavocTutoEndModal = createModal('tuto-end', false);
    setModalTitle(cavocTutoEndModal, '\u{1F3C6} SUCCESS');
    setModalMessage(cavocTutoEndModal,
        'You have completed the tutorial. Congratulations!');
    addModalButton(cavocTutoEndModal, 'Restart the tutorial', (_) => {
        closeModal(cavocTutoEndModal);
        cavocLoadLevel(1);
    });
    addModalButton(cavocTutoEndModal, 'Continue to free play', (_) => {
        closeModal(cavocTutoEndModal);
        cavocExitTutorial();
    });
}

/* --- deep links: the URL fragment names a scenario ------------------------
   #<dir> loads that example of the manifest; #tuto enters the tutorial at
   the stored level, #tuto/<n> at level n. Programmatic updates go through
   history.replaceState, which never fires hashchange, so routing only ever
   answers the user (a shared link, the back button, a typed fragment). */

function cavocRouteHash() {
    const fragment = decodeURIComponent(location.hash.slice(1));
    if (!fragment || !cavocManifest) return false;
    const tuto = fragment.match(/^tuto(?:\/([0-9]+))?$/);
    if (tuto) {
        const level = tuto[1] ? Number(tuto[1]) : undefined;
        if (cavocTutorial) cavocLoadLevel(level || cavocTutorial.level);
        else cavocEnterTutorial(level);
        return true;
    }
    if (cavocManifest.examples.some((e) => e.dir === fragment)) {
        const select = document.getElementById('scenario-select');
        select.value = fragment;
        /* Exiting the tutorial reloads the picker's scenario itself. */
        if (cavocTutorial) cavocExitTutorial();
        else cavocLoadPickedScenario(fragment);
        return true;
    }
    return false;
}

/* --- page initialization -------------------------------------------------
   Runs after explore_web.bc.js: the OCaml side has installed its handlers
   (and exported cavocToggleDebug) by then. */

function cavocPageInit() {
    cavocWireOptionsMenu();
    cavocApplyOptionsToMenu();
    /* The chattering pace is not an LTS option — bin/compose_driver.ml reads
       window.cavocChattering, not cavocScenario.options — so its section of
       the menu is wired on its own, and once: its widgets are static markup. */
    cavocWireChatterControl();
    cavocBuildModals();

    document.getElementById('guide-btn').addEventListener('click', cavocToggleGuide);
    document.getElementById('guide-close').addEventListener('click', cavocToggleGuide);
    cavocLoadGuide('help.md');

    const debugCheck = document.getElementById('debug-log-check');
    debugCheck.addEventListener('change', (e) => {
        cavocToggleDebug(e.target.checked);
    });

    const select = document.getElementById('scenario-select');
    select.addEventListener('change', () => {
        cavocStopIfRunning();
        cavocLoadPickedScenario(select.value);
    });

    document.getElementById('tutorial-btn').addEventListener('click', () => {
        if (cavocTutorial) cavocExitTutorial();
        else cavocEnterTutorial();
    });

    window.addEventListener('hashchange', () => {
        cavocStopIfRunning();
        cavocRouteHash();
    });

    /* The badge offering to stop the local server only shows when one
       answers (a 200-fallback static host answers HTML, not a JSON
       array). */
    fetch('../list_files?dir=test')
        .then((response) => response.json())
        .then((listing) => {
            if (Array.isArray(listing))
                document.getElementById('shutdown-btn').style.display = '';
        })
        .catch(() => {});

    cavocPopulateExamples().then(() => {
        if (cavocRouteHash()) return;
        const singles = cavocManifest
            ? cavocManifest.examples.filter((e) => e.mode === 'single')
            : [];
        if (singles.length > 0) {
            select.value = singles[0].dir;
            cavocLoadPickedScenario(singles[0].dir);
        } else {
            /* No manifest reachable: an empty scratch card. */
            cavocLoadScenario(window.cavocScenario);
        }
    });
}
