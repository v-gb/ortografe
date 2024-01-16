const b = window.chrome ? chrome : browser;
const is_manifest_v2 = false

const fields = ['rewrite', 'disable_watch', 'color', 'trivial', 'debug_changes', 'debug_language'];
const all_fields = ['disable'].concat(fields)

async function saveOptions(e) {
    e.preventDefault();
    const options = {}
    for (f of fields) {
        if (f == 'rewrite') {
            options[f] = document.querySelector('input[name="rewrite-radio"]:checked').value || 'erofa';
        } else {
            options[f] = document.getElementById(f.replace("_", "-") + "-checkbox").checked
        }
    }

    console.log('storing', options)
    await b.storage.local.set(options);
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
}

async function restoreOptions() {
    try {
        const options =
            // b.storage.local.get always returns a promise in rewrite.js,
            // while here we only get a promise with manifest v3??
              await (is_manifest_v2
                     ? new Promise((resolve) => b.storage.local.get(all_fields, resolve))
                     : b.storage.local.get(all_fields))
        console.log('loaded', options)
        normalize_options(options)
        for (f of fields) {
            if (f != 'disable' && f != 'rewrite') {
                document.getElementById(f.replace("_", "-") + "-checkbox").checked =
                    options[f] || false;
            }
        }
        document.getElementById("rewrite-" + options.rewrite).checked = true;
    } catch (e) {
        // any exception is debuggable in firefox, so just include it in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + b.storage.local.get.toString();
        throw e
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
document.querySelector("form").addEventListener("submit", saveOptions);
