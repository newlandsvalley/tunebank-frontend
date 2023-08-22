"use strict";


export function resetHamburgerMenu () {
    const hamburgerMenu = document.getElementById('hamburger-menu');
    const hamburgerLines = document.getElementById('hamburger-lines');
    const hamburgerCross = document.getElementById('hamburger-cross');

    hamburgerMenu.classList.add('hidden');
    hamburgerMenu.classList.remove('flex');

    hamburgerLines.classList.add('visible');
    hamburgerLines.classList.remove('hidden');
    hamburgerCross.classList.add('hidden');
    hamburgerLines.classList.remove('visible');
}

export function toggleHamburgerMenu () {
    const hamburgerMenu = document.getElementById('hamburger-menu');
    const hamburgerLines = document.getElementById('hamburger-lines');
    const hamburgerCross = document.getElementById('hamburger-cross');

    hamburgerMenu.classList.toggle('flex');
    hamburgerMenu.classList.toggle('hidden');

    hamburgerLines.classList.toggle('visible');
    hamburgerLines.classList.toggle('hidden');
    hamburgerCross.classList.toggle('visible');
    hamburgerCross.classList.toggle('hidden');
}