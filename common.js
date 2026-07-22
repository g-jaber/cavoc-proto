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
