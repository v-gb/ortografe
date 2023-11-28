const b = window.chrome ? chrome : browser;
const is_manifest_v2 = false

async function saveOptions(e) {
    e.preventDefault();
    const options = {
	disable: document.querySelector("#disable-checkbox").checked,
	disable_watch: document.querySelector("#disable-watch-checkbox").checked,
	color: document.querySelector("#color-checkbox").checked,
	trivial: document.querySelector("#trivial-checkbox").checked,
        debug_changes: document.querySelector("#debug-changes-checkbox").checked,
        debug_language: document.querySelector("#debug-language-checkbox").checked,
    }
    console.log('storing', options)
    await b.storage.local.set(options);
}

async function restoreOptions() {
    try {
        const fields = ['disable', 'disable_watch', 'color','trivial', 'debug_changes', 'debug_language'];
        const options =
            // b.storage.local.get always returns a promise in rewrite.js,
            // while here we only get a promise with manifest v3??
              await (is_manifest_v2
                     ? new Promise((resolve) => b.storage.local.get(fields, resolve))
                     : b.storage.local.get(fields))
        console.log('loaded', options)
        document.querySelector("#disable-checkbox").checked = options.disable || false;
        document.querySelector("#disable-watch-checkbox").checked = options.disable_watch || false;
        document.querySelector("#color-checkbox").checked = options.color || false;
        document.querySelector("#trivial-checkbox").checked = options.trivial || false;
        document.querySelector("#debug-changes-checkbox").checked = options.debug_changes || false;
        document.querySelector("#debug-language-checkbox").checked = options.debug_language || false;
    } catch (e) {
        // any exception is debuggable in firefox, so just include it in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + b.storage.local.get.toString();
        throw e
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
document.querySelector("form").addEventListener("submit", saveOptions);
