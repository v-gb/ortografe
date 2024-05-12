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
const lazy_dict_gen = async_lazy(async () => {
    await getScript("/static/dict_gen.bc.js");
    return dict_gen;
})
if (user_text2) {
    mirror_and_rewrite(user_text2, converted_text2, async () => {
        const dict_gen = await lazy_dict_gen();
        while (true) {
            // loop to ensure we reach a fixpoint if the selection changes while we compute a
            // dictionary
            const [ rules, selection_text ] = dict_gen.currently_selected_rules("conv-");
            if (selection_text == cache2?.selection_text) {
                break;
            } else {
                // ideally, we would generate the dictionary on demand, like we do when
                // converting docs but that code works on text, not the dom, and can't
                // highlight things.
                [ cache2.dict, cache2.stats ] =
                    await dict_gen.generate("/static", "/static/Lexique383.gen.tsv",
                                            "/static/rect1990.csv", rules, 1, false);
                cache2.selection_text = selection_text;
                cache2.table = null;
            }
        }
        const options = {color:true, trivial:false, background_color:'#b9f4b9',
                         rewrite: 'custom', custom_dict: cache2.dict}
        if (cache2?.table == undefined) {
            cache2.table = load_dict(options);
        }
        // hopefully there can be no "context switch" at the place where the caller
        // awaits this return, because that would introduce a (very tight) race condition
        // that could theorically cause an input change to be missed.
        return [ options, cache2.table ]
    })
}

const lazy_doc_conversion = async_lazy(async () => {
    await getScript("/static/doc_conversion.bc.js");
    return doc_conversion;
})
const lazy_conv = async_lazy(async () => {
    const doc_conversion = await lazy_doc_conversion();
    return doc_conversion.convert('doc-conv-button-error',
                                  'alice,saucissonette\nlapin,lapinou\nanimaux,animals\n')
})
const doc_conv_button = document.getElementById('doc-conv-button')
if (doc_conv_button) {
    doc_conv_button.addEventListener("change", async (e) => {
        // problem: can't send the same file twice ! Should unset the data or something.
        e.preventDefault();
        const conv = await lazy_conv();
        await conv(e.target.files.item(0))
    })
    function doc_conv() {
        document.getElementById('doc-conv').style.display = 'unset'
    }
}

for (const elt of document.getElementsByClassName('mailelt')) {
    elt.setAttribute('href', 'mzilto:contzct@orthogrzphe-rztionnelle.info'.replaceAll('z', 'a'))
}

// indicate which browser is being used, to make it easier for non technical people
// to know what to click on
let browser;
if (/Firefox[/]/.test(navigator.userAgent)) {
    browser = 'firefox';
} else if (/Edge[/]/.test(navigator.userAgent)) {
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
