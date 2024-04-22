function rewrite(text, elt) {
    // ideally, we´d be a bit smarter and not rewrite everything on
    // every single keystroke here
    const options = {color:true, trivial:false, background_color:'#b9f4b9'}
    if (table === null) {
        table = load_dict(options);
    }
    elt.textContent = text
    rewrite_under(options,table,elt);
}

function rewrite_custom(text, elt, dict) {
    const options = {color:true, trivial:false, background_color:'#b9f4b9', rewrite: 'custom',
                     custom_dict: dict}
    if (cache2?.table == undefined) {
        cache2.table = load_dict(options);
    }
    elt.textContent = text
    rewrite_under(options,cache2.table,elt);
}

let table = null
const user_text = document.getElementById('user-text')
const converted_text = document.getElementById('converted-text')
rewrite(user_text.value, converted_text)
user_text.oninput = function() {
    rewrite(this.value, converted_text)
    if (user_text.height < converted_text.height) {
        // autogrow the textarea, so if you paste a large text, you end up with
        // two long texts side by side, instead the textarea with a scrollbar
        // and the other text with a long paragraph.
        user_text.height = converted_text.height
    }
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

function async_lazy(f) {
    let res = null;
    return async () => {
        if (res == null) {
            res = f()
        }
        return await res
    }
}

function ignoring_concurrent_calls(f) {
    let processing = false;
    return async function() {
        if (processing) { return };
        processing = true;
        try {
            return await f(...arguments);
        } finally {
            processing = false;
        }
    }
}

let cache2 = {};
const user_text2 = document.getElementById('user-text2')
const converted_text2 = document.getElementById('converted-text2')
const lazy_dict_gen = async_lazy(async () => {
    await getScript("/static/dict_gen.bc.js");
    return dict_gen;
})

user_text2.oninput = ignoring_concurrent_calls(async () => {
    const dict_gen = await lazy_dict_gen();
    while (true) {
        // loop to ensure we reach a fixpoint if the selection changes while we compute a dictionary
        const [ rules, selection_text ] = dict_gen.currently_selected_rules("conv-");
        if (selection_text == cache2?.selection_text) {
            break;
        } else {
            // ideally, we would generate the dictionary on demand, like we do when converting docs
            // but that code works on text, not the dom, and can't highlight things.
            [ cache2.dict, cache2.stats ] =
                await dict_gen.generate("/static", "/static/Lexique383.gen.tsv", "/static/rect1990.csv", rules, 1, false);
            cache2.selection_text = selection_text;
            cache2.table = null;
        }
    }
    rewrite_custom(user_text2.value, converted_text2, cache2.dict)
    if (user_text2.height < converted_text2.height) {
        user_text2.height = converted_text2.height;
    }
})

document.getElementById('mailelt').setAttribute('href', 'mzilto:contzct@orthogrzphe-rztionnelle.info'.replaceAll('z', 'a'))

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
