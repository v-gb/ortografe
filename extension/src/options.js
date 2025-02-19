if (typeof browser == "undefined") {
    globalThis.browser = chrome;
}

const fields = ['rewrite', 'disable_watch', 'color', 'trivial', 'debug_changes', 'debug_language', 'debug_lang_test', 'fren'];
const all_fields = ['disable'].concat(fields);

async function display_dict_preview(restore) {
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
        if (custom_dict?.meta?.src) {
            const dict_link = document.getElementById("dict-link")
            if (!dict_link.value) {
                dict_link.value = custom_dict.meta.src
            }
        }
        if (restore) {
            if (custom_dict?.meta?.desc) {
                const desc = custom_dict.meta.desc;
                for (const bit of desc.split(" ")) {
                    // We manually ensure that the only ids starting with "checkbox-" are the
                    // ones we want to target, to avoid the (very unlikely) possibility of
                    // checking the wrong boxes.
                    const elt = document.getElementById("checkbox-" + bit);
                    if (elt) {
                        elt.checked = true;
                    }
                }
                currently_selected_rules();
            }
        } else {
            elt.classList.remove('highlight')
            void elt.offsetWidth; // trigger reflow, so the animation restarts
            elt.classList.add('highlight')
        }
    } else {
        elt.innerText = "vide";
    }
}

function parse_dict(str, link = undefined) {
    let lines = str.trim().split("\n");
    let meta = {};
    if (link != undefined) {
        meta.src = link;
    }
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
        if (typeof meta_json?.plurals_in_s == "boolean"
            || typeof meta_json?.plurals_in_s == "string") {
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
    const [ rules, selection_text, nonempty ] = dict_gen_browser.currently_selected_rules("checkbox-");
    document.getElementById("load-checkbox-label-sel").innerText = selection_text;
    document.getElementById("deselect").style.display = nonempty ? "unset" : "none";
    return rules;
}

function add_rule_selection_ui() {
    // The html is static, meaning the possible values are shipped in the extension.
    document.getElementById("load-dict-details").innerHTML = dict_gen_browser.html_fragment();
    document.getElementById("checkbox-custom").addEventListener("input", (e) => {
        with_exn_in_dom("load_error", async () => {
            currently_selected_rules();
        })
    })
}

async function compute_dict(rules, set_progress) {
    const [dict, stats] = await dict_gen_browser.generate("./Lexique383.gen.tsv", "./dict1990.gen.csv", rules, false, false, set_progress);
    console.log(stats);
    return dict;
}

async function load_dict_from_disk(e) {
    if (e.target.files.length > 0) {
        try {
            const file = e.target.files.item(0);
            set_dict(parse_dict(await file.text()));
            document.getElementById("load_error").innerText = "";
            await display_dict_preview(false);
        } catch (e) {
            document.getElementById("load_error").innerText = "error importing file " + e.toString();
        }
    }
}

async function load_dict_from_computation(e) {
    try {
        const rules = currently_selected_rules();
        if (e.target.id == 'load-checkbox') {
            const elt = document.getElementById("load-checkbox-label");
            if (elt.style.getPropertyValue("--progress") == "0%") {
                function set_progress(v) {
                    elt.style.setProperty("--progress", v + "%")
                }
                set_progress(1);
                try {
                    const dict = await compute_dict(rules, set_progress);
                    set_dict(parse_dict(dict));
                    await display_dict_preview(false);
                } finally {
                    set_progress(0);
                }
            }
        }
        document.getElementById("load_error").innerText = "";
    } catch (e) {
        document.getElementById("load_error").innerText = "error loading dict " + e.toString();
    }
}

async function save_options() {
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

async function form_change(e) {
    e.preventDefault();
    // console.log("target", e.target)
    if (e.target.id == 'load-dict-input') {
        await load_dict_from_disk(e)
    } else if (e.target.id.startsWith("checkbox-") || e.target.id == "load-checkbox") {
        await load_dict_from_computation(e)
    } else {
        await save_options()
    }
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
}

function debug_information() {
    const manifest = browser.runtime.getManifest();
    const json = {
        "version": manifest.version,
        "manifest_version": manifest.manifest_version,
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
        await display_dict_preview(true);
    } catch (e) {
        // exceptions get swallowed in firefox, making them undebuggable, so just include
        // them in the page itself
        document.getElementById("error").textContent = e.toString() + "\n" + e.stack + browser.storage.local.get.toString();
        throw e;
    }
}

async function download_dict(link) {
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
    set_dict(parse_dict(dict, link));
    await display_dict_preview(false);
}

async function with_exn_in_dom(id, f) {
    document.getElementById(id).textContent = "";
    try {
        return await f();
    } catch (e) {
        document.getElementById(id).textContent =
            (new Date()).toLocaleTimeString() + ": " + e.toString();
        throw e;
    }
}

function download_dict_click(e) {
    e.preventDefault();
    with_exn_in_dom("load_error", async () => {
        const link = document.getElementById("dict-link").value;
        await download_dict(link)
    })
}

function default_highlighting() {
    // You can check if the user prefers dark mode, and maybe if the site supports dark
    // mode, but you can't really ask "is this site using dark mode now?". So we just
    // check the background color. It would be more principled to put a class on the rewritten
    // text, but good enough for now.
    const color = window.getComputedStyle(document.body).getPropertyValue('background-color');
    const color_components = color.match(/\d+/g);
    if (color_components
        && color_components.length == 3
        && color_components.reduce((a, b) => a + (b | 0),0) < 255 / 2 * 3
       ) {
        return '#106410'
    }
    return '#b9f4b9'
}

async function suggest_dict_from_active_tab() {
    const tabs = await browser.tabs.query({currentWindow: true, active: true});
    const tab = tabs?.[0];
    if (tab) {
        const link =
              false
              ? "https://www.sinplegraf.org/DataJS/sinple.js"
              : await browser.tabs.sendMessage(tab.id, "dictionary-url");
        if (link) {
            document.getElementById("load-from-page").removeAttribute("style");
            {
                const url_elt = document.getElementById("load-from-page-url");
                url_elt.innerText = (new URL(link)).hostname.replace(/^www[.]([^.]+[.][^.]+)$/, (_, s) => s);
                url_elt.setAttribute("href", link);
            }
            document.getElementById("load-from-page-button").
                addEventListener("click", () => {
                    with_exn_in_dom("load_error", async () => {
                        await download_dict(link);
                        document.getElementById("rewrite-custom").checked = true;
                        await save_options();
                    })
                })
        }
    }
}

document.addEventListener('DOMContentLoaded', restoreOptions);
for (const elt of document.querySelectorAll(".form-onchange")) {
    elt.addEventListener("change", form_change);
}
document.getElementById("dict-link-form").addEventListener("submit", download_dict_click);
const open_options_page_elt = document.getElementById("open-options-page");
open_options_page_elt.addEventListener("click", (e) => {
    e.preventDefault();
    browser.runtime.openOptionsPage()
});
// On firefox on computer, the "#popup" is all we need, to distinguish between the popup
// window and full blow options page. On firefox on android though, the popup page is
// already full screen and trying to open the openOptionsPage does something like opening
// a new tab but doesn't make you navigate to it, which is not convenient. We detect
// firefox-on-android with the window size check.
if (location.hash === '#popup' && window.screen.width != window.innerWidth) {
    for (const elt of document.getElementsByClassName("only-if-large")) {
        elt.style.display = "none";
    }
} else {
    open_options_page_elt.style.display = "none";
}
document.documentElement.style.setProperty("--highlight-color", default_highlighting())
{
    const classname = browser.i18n.getMessage("classname");
    const stylesheet = document.styleSheets[0];
    stylesheet.insertRule(`.fr { display: none; }`, stylesheet.cssRules.length)
    stylesheet.insertRule(`.${classname} { display: unset; }`, stylesheet.cssRules.length);
    for (const el of document.querySelectorAll("[placeholder_" + classname + "]")) {
        el.setAttribute("placeholder", el.getAttribute("placeholder_" + classname))
    }
}
suggest_dict_from_active_tab()

function download(filename, text) {
    // https://ourcodeworld.com/articles/read/189/how-to-create-a-file-and-generate-a-download-with-javascript-in-the-browser-without-a-server
    const element = document.createElement('a');
    element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
    element.setAttribute('download', filename);
    element.style.display = 'none';
    document.body.appendChild(element);
    try {
        element.click();
    } finally {
        document.body.removeChild(element);
    }
}

document.getElementById("download-dict").addEventListener("click", async (e) => {
    // make it possible to download the computed dictionaries, as an easy way to let
    // people combine generated dictionaries and custom ones. It'd be nice to have
    // direct support for that, but the UI questions are unclear, and it seems sensible
    // to give people the option to look at what's generated anyway.
    e.preventDefault();
    with_exn_in_dom("load_error", async () => {
        const { custom_dict } = (await browser.storage.local.get('custom_dict'));
        if (custom_dict?.data) {
            const text = JSON.stringify(custom_dict.meta) + "\n" + custom_dict.data;
            download("dict", text);
        } else {
            throw new Error(`rien à télécharger (pas de dictionnaire perso)`);
        }
    })
})

document.getElementById("deselect").addEventListener("click", (e) => {
    for (const elt of document.querySelectorAll('input[id^="checkbox-"]:checked')) {
        elt.checked = false;
    }
    currently_selected_rules();
})
