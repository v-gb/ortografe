#!/usr/bin/env -S python3 -B

import csv
import glob
import json
import re
import sys

def or_key_error(f):
    try:
        return f()
    except KeyError:
        return None

min_gen_csv = []
seen_words = set()
seen_words_with_tags = set()
pre1990 = []
post1990 = {}
dont_want = re.compile('[-0-9.+()&/ ]')
extracted_files = list(sorted(glob.glob("extracted-*.gen.json")))
debug = set(s for arg in sys.argv[1:] for s in arg.split(','))

for line in open(extracted_files[-1]):
    if not line.startswith("{"):
        continue
    j = json.loads(line)
    if j.get("word") in debug:
        print(j)
    if (
            (word := j.get("word")) is not None
            and not dont_want.search(word)
            and (ipa := or_key_error(lambda: j["sounds"][0]["ipa"])) is not None
    ):
        tags = j.get("tags")
        first_instance = word not in seen_words
        seen_words.add(word)
        first_instance_with_tags = tags is not None and word not in seen_words_with_tags
        if tags is not None:
            seen_words_with_tags.add(word)
        # there is no information in general that ties together the various spellings of a word, so
        # we line up such words on their phonetic. We run into homophones (envoûter, envoûté), or
        # multiple traditional spellings that each have their own rectified spellings (péquenot
        # péquenaud -> pèquenot pèquenaud).
        rect = any("orthographe rectifiée de 1990" in s for s in tags or [])
        trad = any("orthographe traditionnelle" in s for s in tags or [])
        h_aspire = any("h aspir" in s for s in tags or [])
        if j.get("word") in debug:
            print({'word':word, 'rect':rect, 'trad':trad, 'h_aspire':h_aspire, 'fi': first_instance, 'fi_tags':first_instance_with_tags, 'ipa':ipa})
        if first_instance:
            min_gen_csv.append((word, ipa, str(h_aspire).lower()))
        if trad or rect:
            key = (ipa, word.endswith("s"), word.endswith("x"),)
            if trad:
                if first_instance_with_tags:
                    # punch has two definitions, and only the second one is modified by the 1990
                    # reform. In such a case, we do not return a mapping from punch->ponch, because
                    # we consider that the first definition is the most important one.
                    pre1990.append((key, word))
            else:
                # There are multiple meanings for "boite", and the first one is not the the post1990
                # spelling of "boîte" one. So consult all definitions to find new spellings. But not
                # to find old spellings, as we don't want to rewrite unrelated words.
                post1990.setdefault(key, set()).add(word)

def csv_writer(fd):
    return csv.writer(fd, delimiter=",", quotechar="\"", quoting=csv.QUOTE_MINIMAL,
                      lineterminator='\n')

min_gen_csv_writer = csv_writer(open('min.gen.csv', 'w'))
min_gen_csv.sort(key=lambda j: j[0])
min_gen_csv_writer.writerow(('word','ipa','h_aspire'))
for row in min_gen_csv:
    min_gen_csv_writer.writerow(row)

def closest_word(trad_word, rect_words):
    if len(rect_words) == 1:
        return next(iter(rect_words))
    from difflib import SequenceMatcher
    rect_word = max(rect_words, key=lambda w: SequenceMatcher(None, trad_word, w).ratio())
    print(f'fyi, possible collision for {trad_word}: chose {rect_word} from {rect_words}')
    return rect_word

def gen_1990():
    for (key, trad_word) in pre1990:
        if (rect_words := post1990.get(key)) is not None:
            if trad_word == 'lieder':
                # special case, otherwise we get lieder
                # This also indicates that we aren't listing plurals for foreign
                # words. Maybe these are more likely to have structured information?
                rect_word = 'lieds'
            else:
                rect_word = closest_word(trad_word, rect_words)
            if trad_word.endswith("y") and rect_word == trad_word[:-1] + "ie":
                # royalty -> royaltie, nursery -> nurserie seem dubious
                continue
            yield (trad_word,rect_word)

csv_1990 = csv_writer(open('1990.gen.csv', 'w'))
csv_1990.writerow(('pre1990','post1990'))
for elt in sorted(gen_1990()):
    csv_1990.writerow(elt)
