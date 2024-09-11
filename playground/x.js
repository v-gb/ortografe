"use strict"

const main_elt = document.getElementById('main')
const observer = new MutationObserver((mutations) => {
    console.log("batch", mutations.length)
    for (const m of mutations) {
        console.log(m);
    }
})
observer.observe(main_elt, { childList: true, characterData: true, subtree: true });

async function pause() {
    await new Promise((resolve) => setTimeout(resolve, 0))
}

async function main() {
    const span_elt = document.createElement('span')
    main_elt.appendChild(span_elt)
    await pause()
    span_elt.textContent = 'asd'
    await pause()
    span_elt.textContent = 'asd qwe'
}

main()
