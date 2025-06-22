async function main() {
    const fs = require('node:fs/promises');
    const src = process.env["SRC"]
    const src_text = await fs.readFile(src, 'utf8')

    const updates = []
    function expect(input, expect, base_entropy) {
        const res = probably_en_rather_than_fr(input, base_entropy || 0)
        const lang = (res[0] ? 'EN' : 'FR');
        const output = `${lang} ${res[1]}`
        if (output != expect) {
            const input_end = src_text.indexOf(input) + input.length + 2;
            const expect_start = src_text.indexOf("\"", input_end);
            const expect_end = src_text.indexOf("\"", expect_start + 1) + 1;
            updates.push([expect_start, expect_end, JSON.stringify(output)]);
        }
    }
    function compute_correction() {
        let i = 0
        let buf = ""
        function flush(next) {
            buf += src_text.substring(i, next);
            i = next;
        }
        for (const [start, end, repl] of updates) {
            flush(start);
            buf += repl;
            i = end;
        }
        flush()
        updates.length = 0
        return buf
    }

    expect("Comment stocker des fichiers sur de l'ADN", "EN -2.4")
    // bad. Because of "ck"
    
    expect("Comment Taiwan domine le monde", "EN -2.7")
    // bad. Because of "wa"

    expect("Perte d'habitat", "FR 1") // good
    expect("Support pour d'autres orthographes", "FR 4") // good

    expect("Formats photos", "EN -9")
    // bad, but quite difficult, as both words are English and French

    expect("an alien intelligence", "FR 3") // bad
    expect("Modifions les prototypes", "EN -5.5") // bad
    expect("Prototypes", "EN -7.7") // Impossible without more context
    expect("Analyse", "EN -5.7") // Impossible without more context
    expect("Dynamite", "FR 6.2,base_entropy,10", 10)

    // console.log(collides_with_english(
    //     { text_is_en: null,
    //       node: { nodeValue: "Introducing Userp - a batteries included user authentication crate inspired by Next Auth" }
    //     }, 'batteries'))

    const correction = compute_correction()
    await fs.writeFile(src + '.corrected', correction)
}

main()
