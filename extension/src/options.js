if (typeof browser == "undefined") {
    globalThis.browser = chrome;
}
const is_manifest_v2 = false;

const fields = ['rewrite', 'disable_watch', 'color', 'trivial', 'debug_changes', 'debug_language', 'debug_lang_test'];
const all_fields = ['disable'].concat(fields);

async function display_dict_preview() {
    const { custom_dict } = (await browser.storage.local.get('custom_dict'));
    const elt = document.getElementById("dict");
    if (custom_dict?.data) {
        const lines = custom_dict.data.trim().split('\n');
        if (lines.length > 0 && lines[0].length > 20000) {
            // assuming it's the old '/' separated format, instead of newline separated
            elt.innerText = "vide";
        } else if (custom_dict?.meta?.desc) {
            elt.innerText = custom_dict.meta.desc;
        } else {
            elt.innerText = `"${lines[0]}" + ${lines.length - 1} lignes`;
        }
    } else {
        elt.innerText = "vide";
    }
}

function parse_dict(str) {
    let lines = str.trim().split("\n");
    let meta = {};
    if (lines.length > 0 && lines[0].startsWith("{")) {
        const meta_json = JSON.parse(lines.shift());
        if (typeof meta_json?.desc == "string") {
            meta.desc = meta_json.desc;
        }
        if (typeof meta_json?.lang == "string") {
            meta.lang = meta_json.lang;
        }
        if (typeof meta_json?.supports_repeated_rewrites == "boolean") {
            meta.supports_repeated_rewrites = meta_json.supports_repeated_rewrites;
        }
        if (typeof meta_json?.plurals_in_s == "boolean") {
            meta.plurals_in_s = meta_json.plurals_in_s;
        }
    }
    for (const line of lines.slice(0, 10)) {
        let num_commas = line.match(/,/g)?.length || 0;
        if (num_commas != 1) {
            throw new Error(`unexpected line (expected one comma per line, found ${num_commas})`);
        }
    }
    const data = lines.join("\n");
    return data ? { 'meta': meta, 'data': data } : null;
}

function count_char(str, c) {
    let count = 0;
    for (const c2 of str) {
        if (c2 == c) {
            count += 1;
        }
    }
    return count;
}

async function set_dict(dict) {
    if (dict?.data) {
        const num_lines = count_char(dict.data, '\n');
        console.log(`storing dict with ${num_lines} entries`);
        await browser.storage.local.set({ 'custom_dict': dict });
    } else {
        await browser.storage.local.remove('custom_dict');
    }
}

function currently_selected_rules() {
    const rules = dict_gen.rules();
    const selected = rules.filter((rule) => document.getElementById(`load-${rule.name}`).checked);
    const load_checkbox_label = document.getElementById("load-checkbox-label");
    const selection_text = selected.length > 0 ? selected.map((r) => r.name).join(' ') : "rien de sélectionné";
    load_checkbox_label.innerText = `Charger la sélection (${selection_text})`;
    return selected.map((r) => r.v);
}

function add_rule_selection_ui() {
    const rules = dict_gen.rules();
    const load_dict_details = document.getElementById("load-dict-details");
    for (const rule of rules) {
        const newnode = document.createElement("div");
        // The html is static, meaning the possible values are shipped in the extension.
        newnode.innerHTML = rule.html;
        load_dict_details.appendChild(newnode);
    }
}

async function compute_dict(rules) {
    const [dict, stats] = await dict_gen.generate("./Lexique383.gen.tsv", "./dict1990.gen.csv", rules, 1, false);
    console.log(stats);
    return dict;
}

async function saveOptions(e) {
    e.preventDefault();
    // console.log("target", e.target)
    if (e.target.id == 'load-dict-input') {
        if (e.target.files.length > 0) {
            try {
                const file = e.target.files.item(0);
                set_dict(parse_dict(await file.text()));
                document.getElementById("load_error").innerText = "";
                await display_dict_preview();
            } catch (e) {
                document.getElementById("load_error").innerText = "error importing file " + e.toString();
            }
        }
    } else if (e.target.id.startsWith("load-")) {
        try {
            const target = e.target.id.substring("load-".length);
            const rules = currently_selected_rules();
            if (target == 'checkbox') {
                const elt = document.getElementById("floatingCirclesG");
                if (elt.classList.contains("idle")) {
                    elt.classList.remove("idle");
                    try {
                        const dict = await compute_dict(rules);
                        set_dict(parse_dict(dict));
                        await display_dict_preview();
                    } finally {
                        elt.classList.add("idle");
                    }
                }
            }
            document.getElementById("load_error").innerText = "";
        } catch (e) {
            document.getElementById("load_error").innerText = "error loading dict " + e.toString();
        }
    } else {
        const options = {};
        for (const f of fields) {
            if (f == 'rewrite') {
                options[f] = document.querySelector('input[name="rewrite-radio"]:checked').value || 'erofa';
            } else {
                options[f] = document.getElementById(f.replaceAll("_", "-") + "-checkbox").checked;
            }
        }
        console.log('storing', options);
        await browser.storage.local.set(options);
    }
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
}

function debug_information() {
    const json = {
        manifest_version: is_manifest_v2 ? "2" : "3",
        "window.screen.width": window.screen.width,
        "window.innerWidth": window.innerWidth,
        "location.hash": location.hash,
    };
    document.getElementById('debug-info').innerText = JSON.stringify(json, null, 2);
}

async function restoreOptions() {
    try {
        debug_information();
        const options = await browser.storage.local.get(all_fields);
        console.log('loaded', options);
        normalize_options(options);
        for (const f of fields) {
            if (f != 'disable' && f != 'rewrite') {
                document.getElementById(f.replaceAll("_", "-") + "-checkbox").checked =
                    options[f] || false;
            }
        }
        document.getElementById("rewrite-" + options.rewrite).checked = true;
        add_rule_selection_ui();
        await display_dict_preview();
    } catch (e) {
        // exceptions get swallowed in firefox, making them undebuggable, so just include
        // them in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + browser.storage.local.get.toString();
        throw e;
    }
}

async function download_dict(e) {
    e.preventDefault();
    document.getElementById("load_error").textContent = "";
    try {
        const link = document.getElementById("dict-link").value;
        if (!link) return;
        if (!link.startsWith("http://") && !link.startsWith("https://")) {
            // prevents grabbing files relative to extension itself, which is weird
            throw new Error(`${link} n'est pas un lien http/https`);
        }
        // It would seem natural to check if we have the permission before requesting, but
        // if we do anything async (like checking if we already have the permissions), we
        // lose the ability to ask for permissions
        // (https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/User_actions)
        // More relevant stuff:
        // https://extensionworkshop.com/documentation/develop/request-the-right-permissions/
        // https://extensionworkshop.com/documentation/develop/test-permission-requests/
        if (!await browser.permissions.request({origins:[link]})) {
            throw new Error(`pas possible de télécharger ${link} sans permission`);
        }
        const response = await fetch(link);
        if (!response.ok) {
            // actually necessary, otherwise you get terrible behavior
            throw new Error(`${response.status} ${response.statusText}`);
        }
        const dict = await response.text();
        set_dict(parse_dict(dict));
        await display_dict_preview();
        document.getElementById("dict-link").value = "";
    } catch (e) {
        document.getElementById("load_error").textContent = (new Date()).toLocaleTimeString() + ": " + e.toString();
        throw e;
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
for (const elt of document.querySelectorAll(".form-onchange")) {
    elt.addEventListener("change", saveOptions);
}
document.getElementById("dict-link-form").addEventListener("submit", download_dict);
const open_options_page_elt = document.getElementById("open-options-page");
const load_dict = document.getElementById("load-dict-section");
// On firefox on computer, the "#popup" is all we need, to distinguish between the popup
// window and full blow options page. On firefox on android though, the popup page is
// already full screen and trying to open the openOptionsPage does something like opening
// a new tab but doesn't make you navigate to it, which is not convenient. We detect
// firefox-on-android with the window size check.
if (location.hash === '#popup' && window.screen.width != window.innerWidth) {
    open_options_page_elt.addEventListener("click", () => browser.runtime.openOptionsPage());
    load_dict.style["display"] = "none";
} else {
    open_options_page_elt.style["display"] = "none";
}
