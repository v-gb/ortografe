.bail on
.mode csv
.import ../wiktionnaire/min-h.gen.csv w
.import 1990.csv r
create index w_word on w(word);
-- min-h.gen.csv has an entry harakiri, but lexique has an entry for hari-kiri, so
-- we miss this h_aspire. We use 1990.csv to add to w an entry for hari-kiri based
-- on the entry for harakiri.
insert into w(word,h_aspire)
select r.ortho,w.h_aspire from w 
inner join r on r.post90 = w.word
where not exists (select 1 from w where r.ortho = w.word);

.import ../homemade/lexique-extra.csv l
.mode tab
.import Lexique383-full.gen.tsv tmp
update tmp
set phon = l.phon
from l where tmp.ortho = l.ortho;

insert into tmp(ortho,phon,lemme)
select l.ortho,
       l.phon,
       case when l.lemme == '' then l.ortho else l.lemme end
from l
left join tmp on l.ortho = tmp.ortho
where tmp.ortho is null
  and l.ortho not like '#%';
-- transform "à l'aveuglette" into "aveuglette"
update tmp
set ortho = substring(ortho, 6),
    lemme = substring(lemme, 6),
    phon = substring(phon, 4)
where ortho like 'à la %' and phon like 'ala%';
update tmp
set ortho = substring(ortho, 5),
    lemme = substring(lemme, 5),
    phon = substring(phon, 3)
where ortho like 'à l''%' and phon like 'al%';
update tmp
set ortho = substring(ortho, 3),
    lemme = substring(lemme, 3),
    phon = substring(phon, 2)
where ortho like 'à %' and phon like 'a%';
.header on
select ortho,phon,lemme,
       (case
        when ortho like 'h%'
        then
          case
          when ortho like 'hai%'
            or ortho like 'haï%'
            or ortho like 'hach%'
            or ortho like 'handicap%'
            or ortho like 'hargn%'
            or ortho like 'hant%'
            or ortho like 'hât%'
            or ortho like 'harna%'
            or ortho like 'hiérarc%'
            or ortho like 'harass%'
            or ortho like 'hêtrai%'
            or ortho like 'hideu%'
            or ortho like 'hockey%'
            or ortho like 'hulul%'
          then 't'
          when ortho like 'hiéro%'
            or ortho like 'hypo%'
            or ortho like 'hyper%'
            or ortho like 'hystér%'
            or ortho like 'hémo%'
            or ortho like 'halog%'
            or ortho like 'homo%'
            or ortho like 'humani%' -- humaniser, mais pas humant
            or ortho like 'humano%' -- humano, mais pas humant
            or ortho like 'héli%' -- héliporté
            or ortho like 'hérédo'
            or ortho like 'herpé%'
          then 'f'
          when coalesce(w_lemme.h_aspire, w.h_aspire, 'true') = 'true' then 't'
          else 'f'
          end
        else 'f'
        end) as h_aspire
from tmp
left join w on tmp.ortho = w.word
left join w as w_lemme on tmp.lemme = w_lemme.word
where ortho not like '% %'
  and ortho not like '%.%'
  and ortho not like '%ñ%'
  and ortho not like '%ã%'
order by cast (freqfilms2 as real) desc
