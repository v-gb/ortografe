const b = window.chrome ? chrome : browser;

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

function rewrite_word(table, s) {
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
              ? [ uncapitalize(word), capitalize ]
              : [ word, (x) => x ]
        let repl = postprocess(table.get(processed_word))
        if (!repl && is_plural(processed_word)) {
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
                 || node.nodeName == "CODE"
                 || node.nodeName == "SCRIPT"
                 || node.nodeName == "STYLE"
                 || node.nodeName == "style" // happens inside <svg>, on reddit
                 || node.nodeName == "TEXTAREA"
                 || node.isContentEditable)
                ? NodeFilter.FILTER_REJECT
                : NodeFilter.FILTER_SKIP
        });    
}

async function plausibly_french(root, debug) {
    const t1 = performance.now();
    const walk = make_walk(root);
    let buf = ""
    while (buf.length < 10000 && walk.nextNode()) {
        if (/[^\s]/.test(walk.currentNode.nodeValue)) {
            buf += walk.currentNode.nodeValue + "\n";
        }
    }
    if (debug) {
        console.log("language detection", buf)
    }
    const t2 = performance.now();
    const { isReliable, languages } = await b.i18n.detectLanguage(buf);
    // I've seen cases where isReliable:false, which was needlessly conservative
    const t3 = performance.now();
    console.log(`detected language: DOM traversal: ${t2 - t1}ms, detection: ${t3 - t2}ms, isReliable: ${isReliable}`, languages.map((e) => e.language + '=' + e.percentage).join(' '))
    // rewrite even if french is not the main language, because oftentimes there is a fair
    // amount of english in the doc that distorts even pages that visually contain mostly french
    return languages.some((l) => l.language == 'fr' && l.percentage >= 30)
}

async function rewrite_under(options, table, root){
    let count = 0
    let to_remove = []
    try {
        const walk = make_walk(root);
        while(n=walk.nextNode()) {
            let regular_text = ""
            // we want to split on words, although there's some ambiguity as to
            // what that means
            // - l'analyse => ["l", "'", "analyse"], otherwise we won't rewrite
            // - indo-européenne => ["indo", "-", "européenne"], otherwise we won't rewrite
            // - truc.com/bidule => ["truc.com/bidule"] but truc. com => ["truc", " ."; "com"]
            // - non breakable space should be a word boundary, same as regular spaces
            const words1 = n.nodeValue.split(/(\p{L}+(?:[-:./_0-9?]+\p{L}+)*)/u)
            for (word1 of words1) {
                const words2 =
                      // if we have a word made purely of letters and dashes (but no slashes,
                      // dots, etc), then actually treat that as multiple words, to handle
                      // the indo-européenne case above
                      word1.includes("-") && /^[-\p{L}]*$/u.test(word1)
                      ? word1.split(/(-)/)
                      : [ word1 ]
                for (word of words2) {
                    const repl = rewrite_word(table, word)
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
                        span.style.backgroundColor = 'orange'
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
    } catch (e) {
        console.log('what', e)
    }
}

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
    var observer = new MutationObserver((mutations) => {
        if (debug) { last_changes.push(mutations) };
        record_change(mutations.length)
    });
    observer.observe(root, { childList: true, characterData: true, subtree: true });
}

function load_dict(options) {
    let table = new Map()
    if (!options.trivial) {
        const t1 = performance.now();
        for (line of fr_dict.split("/")) {
            let [a,b] = line.split(",")
            if (a && b && a != b) {
                table.set(a, b)
            }
        }
        const t2 = performance.now();
        console.log(`reading table: ${t2 - t1}ms`)
    }
    return table
}

async function rewrite_main() {
    const before_storage = performance.now()
    const options = await b.storage.local.get(
        ['disable', 'disable_watch', 'color','trivial', 'debug_changes', 'debug_language']
    )
    const after_storage = performance.now();
    console.log(`reading options: ${after_storage - before_storage}ms`, options)
    if (options.disable) { return };
    const table = load_dict(options);
    // start at document.body, because iterating over document means
    // iterating over <style> nodes even though they contain nothing,
    // and setting their nodeValue messes up pages for some reason
    const root = document.body;
    if (!(await plausibly_french(root, options.debug_language))) { return }
    const num_changes = await rewrite_under(options, table, root);
    const after_rewrite = performance.now()
    console.log(`rewriting all texts: ${num_changes} changes in ${after_rewrite - after_storage}ms`)
    if (!options.disable_watch) {
        watch_for_changes(options, table, root)
    }
}

rewrite_main()
