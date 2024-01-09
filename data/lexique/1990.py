#!/usr/bin/env -S python3 -B

# Computes the 1990 spelling for all the words in lexique 3.83 by
# asking the site https://uclouvain.be/recto-verso
#
# This creates the columns type,post90 in the csv, all the other
# columns are there to uniquely identify the row we're referring to.

import sys
import csv
import requests
from lxml import html
from lxml.html import clean

def load_raw():
    with open("Lexique383/Lexique383.tsv") as file:
        for row in csv.DictReader(file, delimiter="\t", quotechar="\""):
            for f in ["freqlemfilms2", "freqlemlivres", "freqfilms2", "freqlivres"]:
                row[f] = float(row[f])
            yield row

def encoded(ortho):
    return ''.join(map(str,map(ord,ortho))) + "."

def conversion_requests(n):
    count = 0
    for row in load_raw():
        count += 1
        ortho = row["ortho"]
        nombre = row["nombre"]
        genre = row["genre"]
        if row["cgram"] == "NOM":
            poss = []
            if nombre != 'p':
                poss.append((("Une " if genre == 'f' else "Un ") + ortho + " . " + encoded(ortho), []))
            if nombre != 's':
                poss.append(("Des " + ortho + " . " + encoded(ortho), []))
            yield row, poss
        else:
            yield row, [("Zut: " + ortho + " . " + encoded(ortho), [])]
        if count >= n:
            break

def query(query_server, groupid, entries):
    original_text = "\r\n".join(entries)
    with open("/tmp/q-" + groupid, 'w') as f:
        f.write(original_text)
    if not query_server:
        return
    try:
        with open("/tmp/r-" + groupid) as f:
            return f.read().splitlines()
    except FileNotFoundError:
        pass
    page = requests.post('https://uclouvain.be/recto-verso/results.html',
                         data={'original_text':original_text})
    tree = html.fromstring(page.content)
    doc = tree.xpath("//div[@id = 'container']/p")[0]
    for br in doc.xpath("//br"):
        br.tail = "\n" + br.tail if br.tail else "\n"
    text = doc.text_content().strip()
    if 'erreur est survenue' in text:
        raise Exception(text)
    else:
        with open("/tmp/r-" + groupid, 'w') as f:
            f.write(text)
        return text.splitlines()

def output_csv(fd):
    w = csv.writer(fd, delimiter=",", quotechar="\"", quoting=csv.QUOTE_MINIMAL,
                   lineterminator='\n')
    def output_row(row, rewrites):
        ortho = row["ortho"]
        if len(rewrites) > 1 or ortho != rewrites[0]:
            w.writerow((ortho,
                        str(len(rewrites)),
                        "/".join(rewrites),
                        # to uniquely identify rows
                        row["cgram"],
                        row["genre"],
                        row["nombre"],
                        row["infover"],
                        row["lemme"],
                        ))
    w.writerow(('ortho','type','post90', 'cgram', 'genre', 'nombre', 'infover', 'lemme'))
    def process(row, es):
        try:
            # dedup preserving order, so it's always singular first, then plural
            rewrites = list(dict.fromkeys(map(lambda e: e[1][0], es)))
        except Exception as exn:
            print(exn, es, file=sys.stderr)
        output_row(row, rewrites)
    return process

def main():
    if len(sys.argv) < 2:
        count = 1000000
    else:
        count = int(sys.argv[1])
    buf = []
    buf_size = 0
    last_total_inputs = 0
    total_inputs = 0
    inputs = []
    output = output_csv(sys.stdout)
    query_server = True
    def do_query():
        nonlocal buf_size, last_total_inputs
        if buf:
            print("query at inputs", total_inputs, len(buf), file=sys.stderr)
            if False: # (total_inputs > 88577 and total_inputs <= 89644):
                def ffs(r):
                    return '. ZZZZZ . .'
                results = list(map(ffs, buf))
            else:
                groupid = str(last_total_inputs) + "-" + str(total_inputs)
                results = query(query_server, groupid, map(lambda r: r[1], buf))
            if query_server:
                if len(buf) != len(results):
                    print('ERROR', buf, results, file=sys.stderr)
                for ((ortho, sentence, ref), res) in zip(buf, results):
                    rewrite = ' '.join(res.split(' ')[1:-2])
                    if encoded(ortho) != res.split(' ')[-1]:
                        print("ERROR", ((ortho, sentence, ref), res), file=sys.stderr)
                    ref.append(rewrite)
            buf.clear()
            buf_size = 0
        if query_server:
            for (row, es) in inputs:
              output(row, es)
            del inputs[:]
            sys.stdout.flush()
        last_total_inputs = total_inputs
    for (row, es) in conversion_requests(count):
        inputs.append((row, es))
        total_inputs += 1
        for (sentence, ref) in es:
            buf.append((row["ortho"], sentence, ref))
            buf_size += len(sentence)
        if buf_size > 45000 or (total_inputs > 88577 and total_inputs <= 89644):
            do_query()
    do_query()

    # entries = [ "a apparaître", "b inquiétera", "b inquiétera" ]
    # print(query(entries))

main()
