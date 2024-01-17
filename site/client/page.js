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

let table = null
const user_text = document.getElementById('user-text')
const converted_text = document.getElementById('converted-text')
rewrite(user_text.value, converted_text)
user_text.oninput = function() {
    rewrite(this.value, converted_text)
}
