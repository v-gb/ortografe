function async_lazy(f) {
    let res = null;
    return async () => {
        if (res == null) {
            res = f()
        }
        return await res
    }
}

const user_text = document.getElementById('user-text')
const converted_text = document.getElementById('converted-text')
if (user_text) {
    const process = mirror_and_rewrite(user_text, converted_text, async_lazy(async () => {
        const options = {color:true, trivial:false, background_color:'#b9f4b9'}
        const table = load_dict(options);
        return [ options, table ]
    }));
    process();
}

// https://stackoverflow.com/questions/16839698/jquery-getscript-alternative-in-native-javascript
const getScript = url => new Promise((resolve, reject) => {
  const script = document.createElement('script')
  script.src = url
  script.async = true

  script.onerror = reject

  script.onload = script.onreadystatechange = function() {
    const loadState = this.readyState

    if (loadState && loadState !== 'loaded' && loadState !== 'complete') return

    script.onload = script.onreadystatechange = null

    resolve()
  }

  document.head.appendChild(script)
})

let cache2 = {};
const user_text2 = document.getElementById('user-text2')
const converted_text2 = document.getElementById('converted-text2')
const lazy_dict_gen_browser = async_lazy(async () => {
    await getScript("/static/dict_gen_browser.bc.js");
    return dict_gen_browser;
})
if (user_text2) {
    let textarea_has_been_used = false
    const update = mirror_and_rewrite(user_text2, converted_text2, async () => {
        textarea_has_been_used = true;
        const dict_gen_browser = await lazy_dict_gen_browser();
        const dict_blob =
              regles_perso_lazy_dict
              ? await regles_perso_lazy_dict()
              : document.getElementById('form-conv-dict').files[0];
        const word_f =
              await dict_gen_browser.staged_generate(
                  cache2,
                  [ dict_gen_browser.currently_selected_rules("conv-"),
                    "/static/Lexique383.gen.tsv",
                    "/static/rect1990.csv",
                  ],
                  dict_blob,
              )
        const table = { size: 1, has: (word) => word_f(word) != null, get: word_f }
        const options = {color:true, trivial:false, background_color:'#b9f4b9',
                         rewrite: 'custom', custom_dict: null}
        return [ options, table ]
    })
    {
        const input_elt = document.getElementById('form-conv-dict');
        if (input_elt) {
            const update_label = () => {
                const elt = document.getElementById('form-conv-dict-deselect');
                elt.style.display = (input_elt.files[0] == null) ? 'none' : 'unset'
            }
            input_elt.addEventListener("change", update_label);
            update_label();
        }
    }
    const lazy_doc_conversion = async_lazy(async () => {
        await getScript("/static/doc_conversion.bc.js");
        return doc_conversion;
    })
    const lazy_conv = async_lazy(async () => {
        const doc_conversion = await lazy_doc_conversion();
        return doc_conversion.create();
    })
    document.getElementById('form-conv-button')?.addEventListener("change", async (e) => {
        const dict_blob =
              regles_perso_lazy_dict
              ? await regles_perso_lazy_dict()
              : document.getElementById('form-conv-dict').files[0];
        if (!dict_blob) {
            e.target.form.submit();
            return
        }
        // problem: can't send the same file twice ! Should unset the data or something.
        e.preventDefault();
        const label = document.getElementById("form-conv-label");
        function set_progress(v) {
            label.style.setProperty("--progress", v + "%")
        }
        set_progress(0);
        try {
            const dict_gen_browser = await lazy_dict_gen_browser();
            const conv = await lazy_conv();
            await doc_conversion.convert(
                conv,
                'form-conv-button-error',
                [ dict_gen_browser.currently_selected_rules("conv-"),
                  "/static/Lexique383.gen.tsv",
                  "/static/rect1990.csv",
                ],
                dict_blob,
                e.target.files[0],
                set_progress,
            )
        } finally {
            set_progress(0);
        }        
    })
    const update_url = sequentialized_and_merged(async () => {
        const url = new URL(window.location);
        const searchParams = url.searchParams;
        if (search_params.has("regles")) {
            const dict_gen_browser = await lazy_dict_gen_browser();
            const [ _rules, selection_text ] = dict_gen_browser.currently_selected_rules("conv-");
            searchParams.set("regles", selection_text);
            window.history.replaceState(null, "", url);
        }
    })
    document.getElementById('form-conv')?.addEventListener("change", () => {
        if (textarea_has_been_used) {
            // Avoid downloading all the stuff if the user hasn't typed in the
            // textarea yet.
            update();
        }
        update_url();
    })
}

for (const elt of document.getElementsByClassName('mailelt')) {
    elt.setAttribute('href', 'mzilto:contzct@orthogrzphe-rztionnelle.info'.replaceAll('z', 'a'))
}

// indicate which browser is being used, to make it easier for non technical people
// to know what to click on
let browser;
if (/Firefox[/]/.test(navigator.userAgent)) {
    browser = 'firefox';
} else if (/Edg(e|)[/]/.test(navigator.userAgent)) {
    browser = 'edge';
} else if (/Chrom(e|ium)[/]/.test(navigator.userAgent)) {
    browser = 'chrome';
} else if (/Safari[/]/.test(navigator.userAgent)) {
    browser = 'safari';
}
if (browser) {
    for (const elt of document.getElementsByClassName("for-" + browser)) {
        elt.classList.add('for-active-browser')
    }
}

if ((new URL(window.location)).searchParams.get("exp")) {
    for (const elt of Array.from(document.getElementsByClassName('exp-hidden'))) {
        elt.classList.remove('exp-hidden')
        elt.classList.add('exp-shown')
    }
}


document.getElementById("download-dict")?.addEventListener("click", async (e) => {
    e.preventDefault();
    function set_progress(v) {
        e.target.style.setProperty("--progress", v + "%")
    }
    set_progress(0);
    e.target.classList.add("loading");
    try {
        const dict_gen_browser = await lazy_dict_gen_browser();
        set_progress(10);
        const [ rules, selection_text ] = dict_gen_browser.currently_selected_rules("conv-");
        const [ dict, _stats ] =
              await dict_gen_browser.generate("/static/Lexique383.gen.tsv",
                                              "/static/rect1990.csv", rules, false,
                                              (i) => set_progress(10 + i * 8 / 10));
        set_progress(90);
        dict_gen_browser.download_from_memory("text/plain", "dict.csv", dict);
    } finally {
        e.target.classList.remove("loading");
    }
})


// duplicated from rewrite.js
function sequentialized_and_merged(f) {
    let state = 0; // 0: idle, 1: running, 2: running + need to call again afterwards
    return async function() {
        if (state != 0) { state = 2; return };
        try {
            while (true) {
                state = 1;
                await f(...arguments);
                if (state == 1) { return }
            }
        } finally {
            state = 0;
        }
    }
}

const dict_search_input = document.getElementById("dict-search-input");
if (dict_search_input) {
    const dict_search_output = document.getElementById("dict-search-output");
    dict_search_input.addEventListener("input", sequentialized_and_merged(async (e) => {
        let html;
        // Strip leading spaces in case of copy-pasting, as leading spaces will
        // otherwise result in no matches.
        // Trailing spaces on the other hand could match. We remove one because
        // gboard will add a trailing space on completion.
        const text = dict_search_input.value.replace(/^ *(.*?) ?$/, "$1");
        if (text == '') {
            html = ''
        } else {
            const p = new URLSearchParams();
            p.set("q", text);
            const response = (await fetch("/dict?" + p.toString()));
            if (!response.ok) {
                throw new Error(`${response.status} ${response.statusText}`);
            }
            html = await response.text();
        }
        dict_search_output.innerHTML = html;
    }))
}

const regles_perso_link = document.getElementById("regles-perso-link");
let regles_perso_lazy_dict = null;
if (regles_perso_link) {
    const search_params = (new URL(window.location)).searchParams;
    const link = search_params.get("url");
    if (link) {
        if (!link.startsWith("http://") && !link.startsWith("https://")) {
            throw new Error(`${link} n'est pas un lien http/https`);
        }
        regles_perso_link.href = link;
        regles_perso_link.text = (new URL(link)).hostname.replace(/^www[.]([^.]+[.][^.]+)$/, (_, s) => s);
        regles_perso_link.classList.add('orthographe-rationnelle-dict')
        regles_perso_link.setAttribute('orthographe-rationnelle-dict-url', link)

        regles_perso_lazy_dict = async_lazy(async () => {
            const response = await fetch(link);
            if (!response.ok) {
                throw new Error(`${response.status} ${response.statusText}`);
            }
            return await response.blob();
        })
    } else {
        regles_perso_lazy_dict = (() => null);
    }
    const regles = search_params.get("regles");
    if (regles) {
        const custom = []
        for (const bit of regles.split(" ")) {
            const elt = document.getElementById("conv-" + bit);
            if (elt) {
                elt.checked = true;
            } else {
                custom.push(bit)
            }
        }
        const elt = document.getElementById("conv-custom");
        if (elt) {
            console.log('bit', custom)
            elt.value = custom.join(' ');
        }
    }
}
