async function foo() {
    try {
        const lexique383 = await (await fetch("../../_build/default/" + "data/lexique/Lexique383.gen.tsv")).text()
        const rect1990 = await (await fetch("../../_build/default/" + "extension/dict1990.gen.csv")).text()
        const str = zzz_dict_gen(lexique383, rect1990)
        document.write(str)
    } catch (e) {
        document.write(e.toString())
    }
}
foo()
