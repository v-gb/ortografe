async function foo() {
    let fetch;
    if (document) {
        fetch = async function(file) {
            return await (await fetch(file)).text()
        }
    } else {
        const fs = require('node:fs');
        fetch = async function(file) {
            return fs.readFileSync(file, 'utf8');
        }
    }
    const lexique383 = await fetch("_build/default/" + "data/lexique/Lexique383.gen.tsv")
    const rect1990 = await fetch("_build/default/" + "extension/dict1990.gen.csv")
    const [buffer, stats] = module.exports.generate_dict(lexique383, rect1990);
    console.log(stats)
    console.log(buffer)
}
foo()
