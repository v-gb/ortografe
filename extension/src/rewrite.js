function is_capitalized(s) {
    return s && s.charAt(0).toLowerCase() != s.charAt(0)
}

function uncapitalize(s) {
    // should be good enough for french, except perhaps some letters
    // with diacritics
    return s.charAt(0).toLowerCase() + s.slice(1)
}

function capitalize(s) {
    // pass undefined/null through, so it's easier to check on the caller side
    return s ? s.charAt(0).toUpperCase() + s.slice(1) : s;
}

function is_plural(s) {
    return s.endsWith("s")
}

function depluralize(s) {
    return s.slice(0, -1)
}

function pluralize(s) {
    // pass undefined/null through, so it's easier to check on the caller side
    return s ? s + "s" : s
}

function rewrite_word(table, plurals_in_s, s) {
    // unsure if the browser gives any guarantee about NFC vs NFD,
    // because currently we assume diacritics are represented the same
    // in the dict and in the page
    if (table.size == 0) {
        const repl = word.replace("a","X")
        return repl == word ? undefined : repl
    } else {
        // we take plurals and capitalizations into account, but
        // - we miss all conjugated verbs (apparaissent isn't rewritten to aparaissent)
        //   as well any other form kind of derived words
        // - we have no clue about the meaning of words, so we're going to rewrite
        //   héroine into éroine, or Cannes into Canes, whether it makes sense or not
        const [ processed_word, postprocess ] =
              is_capitalized(word)
              ? (table.has(word) // capitalized words in dict are exceptions
                 ? [ '', (x) => x ]
                 : [ uncapitalize(word), capitalize ])
              : [ word, (x) => x ]
        let repl = postprocess(table.get(processed_word))
        if (!repl && is_plural(processed_word) && plurals_in_s) {
            repl = postprocess(pluralize(table.get(depluralize(processed_word))))
        }
        return repl
    }
}

function make_walk(root) {
    // find all bits in the page in the page but:
    // - don't translate <code>, because any change can break code (example: github)
    // - don't translate notranslate, which is used on mdn for inlined bits of code
    // - don't translate anything user writable, like textarea
    return document.createTreeWalker(
        root,
        NodeFilter.SHOW_TEXT | NodeFilter.SHOW_ELEMENT,
        (node) => {
            return node.nodeType == 3
                ? NodeFilter.FILTER_ACCEPT :
                (node.classList.contains("notranslate")
                 || node.classList.contains("notranscribe")
                 || node.nodeName == "CODE"
                 || node.nodeName == "SCRIPT"
                 || node.nodeName == "NOSCRIPT" // contains only text when scripting is enabled (~everyone)
                                                // which messes up language detection. It's not clear how to
                                                // process these nodes only when JS is disabled, so ignore
                 || node.nodeName == "STYLE"
                 || node.nodeName == "style" // happens inside <svg>, on reddit
                 || node.nodeName == "TEXTAREA"
                 || node.isContentEditable)
                ? NodeFilter.FILTER_REJECT
                : NodeFilter.FILTER_SKIP
        });
}

function lazy(f) {
    let res = null;
    return () => {
        if (res == null) {
            res = f()
        }
        return res
    }
}

async function detect_language_with_browser_api(b, lazystr, lang) {
    if (!b?.i18n?.detectLanguage) {
        return  { res: null, fallback: true }
    }
    const [str, dur] = lazystr();
    if (str.length == 0) {
        return { res: null, fallback: false }
    }
    const { isReliable, languages } = await b.i18n.detectLanguage(str)
    // I've seen cases where isReliable:false, which was needlessly conservative.
    //
    // Rewrite even if lang is not the main language, because oftentimes there is a
    // fair amount of english in the doc that distorts even pages that visually
    // contain mostly lang.
    return {
        res: languages.some((l) => l.language == lang && l.percentage >= 30),
        why: [`isReliable:${isReliable}`,
              languages.map((e) => e.language + '=' + e.percentage).join(' '),
              `dom-traversal:${dur}ms`]
    }
}

function detect_language_with_lang_attr(lang) {
    const doclang = document.documentElement.lang;
    if (!doclang) {
        return { res: null, fallback: true }
    }
    // Much simpler and faster than looking at the text, but I think not as good as
    // looking at the language on the page. I feel like something with pure english
    // content and a tiny bit of navbar in lang gets treated as lang.
    return { res: doclang.startsWith(lang), why: [`html.lang:${doclang}`] }
}

function detect_language_custom(lazystr, lang) {
    if (lang != 'fr') {
        return { res: null, fallback: true };
    }
    const [str, dur] = lazystr();
    if (str.length == 0) {
        return { res: null, fallback: false };
    }
    const num_diacritics = str.replace(/[^éèêëàçô]/ug, '').length;
    // 0.5% of diacratics seems like it indicates french quite clearly.  Some pages
    // can have less than that, even.
    return { res : (num_diacritics > 0.005 * str.length),
             why: [`diacritics:${((100 * num_diacritics)/str.length).toFixed(1)}%`,
                   `dom-traversal:${dur}ms`]
           }
}

async function plausibly_lang_once_unlogged(b, lang, root, debug, force_fallback) {
    const lazystr = lazy(() => {
        const t1 = performance.now();
        const walk = make_walk(root);
        let buf = "";
        while (buf.length < 10000 && walk.nextNode()) {
            if (/[^\s]/.test(walk.currentNode.nodeValue)) {
                buf += walk.currentNode.nodeValue + "\n";
            }
        }
        const t2 = performance.now();
        if (debug) {
            console.log("language detection", buf)
        }
        return [buf, t2 - t1]
    })
    let res = { res: null, fallback : true };
    if (res.res == null && res.fallback && !force_fallback) {
        res = await detect_language_with_browser_api(b, lazystr, lang);
    }
    if (res.res == null && res.fallback && !force_fallback) {
        res = detect_language_with_lang_attr(lang);
    }
    if (res.res == null && res.fallback) {
        res = detect_language_custom(lazystr, lang);
    }
    if (res.res == null && res.fallback) {
        res = { res: true, why: ['just guessing'] }
    }
    return res.res == null ? null : res
}

async function plausibly_lang_once(b, lang, root, debug, force_fallback) {
    const t1 = performance.now();
    const res = await plausibly_lang_once_unlogged(b, lang, root, debug, force_fallback);
    const t2 = performance.now();
    console.log(`page is ${lang}:${res ? res.res : "-"}, ${t2 - t1}ms,`, ...(res ? res.why : []))
    return res ? res.res : res
}

async function plausibly_lang(b, lang, root, debug, force_fallback) {
    let delay = 100;
    while (true) {
        const res = await plausibly_lang_once(b, lang, root, debug, force_fallback)
        if (res !== null) {
            return res;
        }
        // for pages that contain nothing like deezer, just wait to figure out the
        // language. If the page contains a placeholder saying "please wait for the page
        // to load or similar, at least that placeholder would inform us of the language,
        // so the fact that we won't run the language detection again seems ok.
        await new Promise((resolve) => {
            const observer = new MutationObserver((_mutations) => {
                observer.disconnect();
                setTimeout(resolve, delay)
                delay = Math.min(delay * 2, 1000)
            })
            observer.observe(root, { childList: true, characterData: true, subtree: true })
        });
    }
}

function rewrite_under(options, table, root){
    let count = 0
    let to_remove = []
    const backgroundColor = options.background_color || '#b9f4b9'
    const plurals_in_s = !!options.plurals_in_s;
    const walk = make_walk(root);
    while(n=walk.nextNode()) {
        let regular_text = ""
        // we want to split on words, although there's some ambiguity as to
        // what that means
        // - l'analyse => ["l", "'", "analyse"], otherwise we won't rewrite
        // - indo-européenne => ["indo", "-", "européenne"], otherwise we won't rewrite
        // - truc.com/bidule => ["truc.com/bidule"] but truc. com => ["truc", " ."; "com"]
        // - non breakable space should be a word boundary, same as regular spaces
        // \p{L} is any unicode letter I think: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Regular_expressions/Unicode_character_class_escape
        const words1 = n.nodeValue.split(/(\p{L}+(?:[-:./_0-9?]+\p{L}+)*)/u)
        for (word1 of words1) {
            const words2 =
                  // if we have a word made purely of letters and dashes (but no slashes,
                  // dots, etc), then actually treat that as multiple words, to handle
                  // the indo-européenne case above
                  word1.includes("-") && /^[-\p{L}]*$/u.test(word1)
                  ? (table.has(word1) // for passe-partout -> passepartout
                     ? [ word1 ]
                     : word1.split(/(-)/))
                  : [ word1 ]
            for (word of words2) {
                const repl = rewrite_word(table, plurals_in_s, word)
                if (!options.color) {
                    regular_text = regular_text + (repl ? repl : word)
                    continue
                }
                if (!repl) {
                    regular_text = regular_text + word
                }
                if (repl && regular_text) {
                    count += 1
                    n.parentNode.insertBefore(document.createTextNode(regular_text), n)
                    regular_text = ""
                }
                if (repl) {
                    count += 1
                    const span = document.createElement('span');
                    span.appendChild(document.createTextNode(repl));
                    span.style.backgroundColor = backgroundColor;
                    n.parentNode.insertBefore(span, n)
                }
            }
        }
        if (n.nodeValue != regular_text) {
            // don't touch the dom if we don't make changes, to avoid what I think
            // is self-triggering in the mutation observer. And it might be faster
            if (!options.color) {
                count += 1
                n.nodeValue = regular_text
            } else {
                if (regular_text) {
                    count += 1
                    n.parentNode.insertBefore(document.createTextNode(regular_text), n)
                }
                to_remove.push(n)
            }
        }
    }
    for (n of to_remove) {
        n.remove()
    }
    return count
}

const synchronous_updates = new Map([
    [ 'www.youtube.com', function(options, table, mutations) {
        let to_rewrite = [];
        for (m of mutations) {
            for (node of m.addedNodes) {
                if (node.nodeType == 1 && node.classList.contains('ytp-caption-segment')) {
                    to_rewrite.push(node)
                }
            }
        }
        for (node of to_rewrite) {
            // The separate loop was to see if it would make the screen update faster
            // after the DOM was updated. Doesn't seem to help though, sadly, but maybe
            // the batching is good anyway. Somehow for hand-written subtitles, this seems
            // to be enough, but not for auto-generated subtitles. We seem to be sometimes
            // missing updates for auto-generated subtitles in fact. Not clear why that is.
            rewrite_under(options, table, node)
        }
    }]
])

function watch_for_changes(options, table, root) {
    // now, for dynamic pages, like reddit or lemonde, rewrite text from times
    // to times. It seems ~impossible to avoid traversing the whole document when rewriting
    // (because even if we know where changes happen, we might be under a notranslate attribute
    // for instance). It seems simpler to just wait for the page to update, and once things have
    // stabilize look at it (i.e. same thing as would happen if a new page was loaded).
    const debug = options.debug_changes;
    let count = 0;
    let time_of_last_rewrite = -10_000;
    let last_changes = [];
    function timeout_fired(delay, count_when_scheduled) {
        return (async function () {
            // Because we traverse the whole page to do any rewrite, it's probably
            // not ideal in terms of performance to rewrite the page immediately on
            // any tiny change. So what we do is:
            // - if the page has changed in the past 100ms, wait 100ms and try again
            // - otherwise, and if the page has changed a lot, just rewrite it
            // - otherwise, rewrite the page if the last rewrite was at least 5s ago
            if (count == count_when_scheduled && (count > 1000 || delay <= 0)) {
                const t1 = performance.now();
                const num_changes = await rewrite_under(options, table, root);
                const t2 = performance.now();
                console.log(`rewriting after ${count} modifications: ${num_changes} changes in ${t2 - t1}ms`)
                if (debug) {
                    console.log(last_changes.slice(-10))
                    console.log(last_changes.slice(-10).map((m) => m.map((m) => m.addedNodes)))
                    last_changes = []
                }
                time_of_last_rewrite = t2
                count = 0;
            } else {
                setTimeout(timeout_fired(delay - 1, count), 100)
            }
        })
    }
    function record_change(n) {
        if (count == 0) {
            const delay = Math.round((5000 + time_of_last_rewrite - performance.now())/100.)
            if (false) { console.log(`detected changes, delay of ${delay}`) }
            setTimeout(timeout_fired(delay, count + n), 100)
        }
        count += n;
    }
    const synchronous_update = synchronous_updates.get(location.hostname)
    const observer = new MutationObserver((mutations) => {
        if (debug) { last_changes.push(mutations) };
        if (synchronous_update) {
            synchronous_update(options, table, mutations)
        }
        // not clear how to avoid looking at the whole page from times to times
        record_change(mutations.length)
    });
    observer.observe(root, { childList: true, characterData: true, subtree: true });
}

function load_dict(options) {
    // should try have a json object and turning it into a map, in case that's faster
    let table = new Map()
    if (!options.trivial) {
        const t1 = performance.now();
        const [ dict, separator ] =
              options.rewrite == 'rect1990' ? [ dict_rect1990, '/' ] :
              options.rewrite == 'custom' ? [ (options.custom_dict || ''), '\n' ] :
              [ dict_erofa, '/' ];
        for (line of dict.split(separator)) {
            const [a,b] = line.split(",")
            if (a && b && a != b) {
                table.set(a, b)
            }
        }
        if (table.size == 0) {
            // table.size is used to indicate the options.trivial elsewhere, so ensure we
            // don't provide such a table
            table.set("f3b0686c-b7b6-11ee-9514-dfbe4f72d338", "wonthapppen")
        }
        const t2 = performance.now();
        console.log(`reading table: ${t2 - t1}ms`)
    }
    return table
}

function normalize_options(options) {
    if (!options.rewrite) {
        options.rewrite = options.disable ? 'disable' : 'erofa';
    }
    if (options?.plurals_in_s != false) {
        options.plurals_in_s = true;
    }
}

async function extension_main() {
    const b = window.chrome ? chrome : browser;
    const before_storage = performance.now()
    const options = await b.storage.local.get(
        ['rewrite', 'disable', 'disable_watch', 'color','trivial', 'debug_changes',
         'debug_language', 'debug_lang_test']
    )
    const halfway_storage = performance.now();
    if (options.rewrite === 'custom') {
        const more_options = await b.storage.local.get(['custom_dict']);
        options.custom_dict = more_options.custom_dict?.data;
        options.lang = more_options.custom_dict?.lang;
        if (more_options.custom_dict?.supports_repeated_rewrites == false) {
            options.disable_watch = true
        }
        if (more_options.custom_dict?.plurals_in_s == false) {
            options.plurals_in_s = false
        }
    }
    normalize_options(options)
    const after_storage = performance.now();
    console.log(`reading options: ${after_storage - before_storage}ms`
                + ` (${after_storage - halfway_storage}ms for custom dict)`,
                { ...options, custom_dict: "#" + options.custom_dict?.length })
    if (options.rewrite == 'disable') { return };
    const table = load_dict(options);
    // start at document.body, because iterating over document means
    // iterating over <style> nodes even though they contain nothing,
    // and setting their nodeValue messes up pages for some reason
    const root = document.body;
    if (!(await plausibly_lang(b, (options.lang || 'fr'), root,
                               options.debug_language,
                               options.debug_lang_test))) {
        return
    }
    const num_changes = await rewrite_under(options, table, root);
    const after_rewrite = performance.now()
    console.log(`rewriting all texts: ${num_changes} changes in ${after_rewrite - after_storage}ms`)
    if (!options.disable_watch) {
        watch_for_changes(options, table, root)
    }
}
