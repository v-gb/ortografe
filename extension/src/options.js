const b = window.chrome ? chrome : browser;
const is_manifest_v2 = false

const fields = ['rewrite', 'disable_watch', 'color', 'trivial', 'debug_changes', 'debug_language', 'debug_lang_test'];
const all_fields = ['disable'].concat(fields)

async function grab_dict(name) {
    // firefox linter is not happy with javascript files over 4MB, and also not happy with
    // json files over 4MB. So we use make one dictionary per file, and it's not even
    // json.
    const response = await fetch("./" + name + ".dict");
    return (await response.text());
}

async function display_dict_preview() {
    const custom_dict = (await storage_get('custom_dict')).custom_dict
    const elt = document.getElementById("dict")
    if (custom_dict?.data) {
        const lines = custom_dict.data.trim().split('\n');
        if (lines.length > 0 && lines[0].length > 20000) {
            // assuming it's the old '/' separated format, instead of newline separated
            elt.innerText = "vide"
        } else if (custom_dict?.meta?.desc) {
            elt.innerText = custom_dict.meta.desc
        } else {
            elt.innerText = `"${lines[0]}" + ${lines.length-1} lignes`
        }
    } else {
        elt.innerText = "vide"
    }
}

function parse_dict(str) {
    let lines = str.trim().split("\n");
    let meta = {};
    if (lines.length > 0 && lines[0].startsWith("{")) {
        const meta_json = JSON.parse(lines.shift())
        if (typeof meta_json?.desc == "string") {
            meta.desc = meta_json.desc;
        }
        if (typeof meta_json?.lang == "string") {
            meta.lang = meta_json.lang;
        }
        if (typeof meta_json?.supports_repeated_rewrites == "bool") {
            meta.supports_repeated_rewrites = meta_json.supports_repeated_rewrites;
        }
    }
    const data = lines.join("\n")
    return data ? { 'meta': meta, 'data': data } : null;
}

async function set_dict(dict) {
    console.log(`storing dict of size ${dict?.data?.length}`)
    if (dict?.data) {
        await b.storage.local.set({'custom_dict': dict});
    } else {
        await b.storage.local.remove('custom_dict');
    }
}

async function saveOptions(e) {
    e.preventDefault();
    // console.log("target", e.target)
    if (e.target.id == 'load-dict-input') {
        if (e.target.files.length > 0) {
            try {
                const file = e.target.files.item(0)
                set_dict(parse_dict(await file.text()));
                document.getElementById("file_error").innerText = "";
                await display_dict_preview()
            } catch (e) {
                document.getElementById("file_error").innerText = "error importing file " + e.toString();
            }
        }
    } else if (e.target.id.startsWith("load-")) {
        try {
            const dict_name = e.target.id.substring("load-".length);
            set_dict(parse_dict(await grab_dict(dict_name)));
            document.getElementById("load_error").innerText = "";
            await display_dict_preview()
        } catch (e) {
            document.getElementById("load_error").innerText = "error loading dict " + e.toString();
        }
    }
    else {
        const options = {}
        for (f of fields) {
            if (f == 'rewrite') {
                options[f] = document.querySelector('input[name="rewrite-radio"]:checked').value || 'erofa';
            } else {
                options[f] = document.getElementById(f.replaceAll("_", "-") + "-checkbox").checked
            }
        }

        console.log('storing', options)
        await b.storage.local.set(options);
    }
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
}

async function storage_get(fields) {
    // b.storage.local.get always returns a promise in rewrite.js,
    // while here we only get a promise with manifest v3??
    return (is_manifest_v2
            ? new Promise((resolve) => b.storage.local.get(fields, resolve))
            : b.storage.local.get(fields))
}

async function restoreOptions() {
    try {
        document.getElementById('manifest-version').innerText = is_manifest_v2 ? "2" : "3"
        const options = await storage_get(all_fields)
        console.log('loaded', options)
        normalize_options(options)
        for (f of fields) {
            if (f != 'disable' && f != 'rewrite') {
                document.getElementById(f.replaceAll("_", "-") + "-checkbox").checked =
                    options[f] || false;
            }
        }
        document.getElementById("rewrite-" + options.rewrite).checked = true;
        await display_dict_preview()
    } catch (e) {
        // exceptions get swallowed in firefox, making them undebuggable, so just include
        // them in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + b.storage.local.get.toString();
        throw e
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
document.querySelector("form").addEventListener("change", saveOptions);
const open_options_page_elt = document.getElementById("open-options-page")
const load_dict = document.getElementById("load-dict-section")
if (location.hash === '#popup') {
    open_options_page_elt.addEventListener("click", () => b.runtime.openOptionsPage());
    load_dict.style["display"] = "none"
} else {
    open_options_page_elt.style["display"] = "none"
}
