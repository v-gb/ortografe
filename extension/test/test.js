async function main() {
    const fs = require('node:fs/promises');
    const src = process.env["SRC"]
    const src_text = await fs.readFile(src, 'utf8')

    const updates = []
    function expect(input, expect) {
        const res = probably_en_rather_than_fr(input)
        const lang = (res[0] ? 'EN' : 'FR');
        const entropy_en = res[1][0].toFixed(1)
        const entropy_fr = res[1][1].toFixed(1)
        const output = `${lang} en:${entropy_en} fr:${entropy_fr}`
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

    expect("Comment stocker des fichiers sur de l'ADN", "EN en:594.7 fr:597.1")
    // bad. Because of "ck"
    
    expect("Comment Taiwan domine le monde", "EN en:400.3 fr:403.0")
    // bad. Because of "wa"

    expect("Perte d'habitat", "FR en:168.9 fr:167.8") // good
    expect("Support pour d'autres orthographes", "FR en:412.0 fr:408.0") // good

    expect("Formats photos", "EN en:146.5 fr:155.5")
    // bad, but quite difficult, as both words are English and French

    expect("an alien intelligence", "FR en:235.9 fr:232.9") // bad
    expect("Modifions les prototypes", "EN en:270.0 fr:275.5") // bad
    expect("Prototypes", "EN en:71.8 fr:79.4") // Impossible without more context
    expect("Analyse", "EN en:44.9 fr:50.5") // Impossible without more context

    // console.log(collides_with_english(
    //     { text_is_en: null,
    //       node: { nodeValue: "Introducing Userp - a batteries included user authentication crate inspired by Next Auth" }
    //     }, 'batteries'))

    const correction = compute_correction()
    await fs.writeFile(src + '.corrected', correction)
}

main()
