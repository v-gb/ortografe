const b = window.chrome ? chrome : browser;
const is_manifest_v2 = false

const fields = ['disable', 'disable_watch', 'color','trivial', 'debug_changes', 'debug_language'];

async function saveOptions(e) {
    e.preventDefault();
    const options = {}
    for (f of fields) {
        options[f] = document.getElementById(f.replace("_", "-") + "-checkbox").checked
    }
    console.log('storing', options)
    await b.storage.local.set(options);
}

async function restoreOptions() {
    try {
        const options =
            // b.storage.local.get always returns a promise in rewrite.js,
            // while here we only get a promise with manifest v3??
              await (is_manifest_v2
                     ? new Promise((resolve) => b.storage.local.get(fields, resolve))
                     : b.storage.local.get(fields))
        console.log('loaded', options)
        for (f of fields) {
            document.getElementById(f.replace("_", "-") + "-checkbox").checked =
                options[f] || false;
        }
    } catch (e) {
        // any exception is debuggable in firefox, so just include it in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + b.storage.local.get.toString();
        throw e
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
document.querySelector("form").addEventListener("submit", saveOptions);
