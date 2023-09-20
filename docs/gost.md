# `Gost` examples

### Books
Input:
```yaml
textbook1:
  type: book
  author: {name: Малиновский, given-name: 'Ю М'}
  title: 'Биосферные основы литологии'
  note: 'Учебное пособие'
  location: Санкт-Петербург
  publisher: ИПК РУДН
  date: 2003
  page-range: 111
```
Output:

`Малиновский, Ю. М. Биосферные основы литологии : Учебное пособие / Ю. М. Малиновский. - СПБ.: ИПК РУДН, 2003. - 111 с.`

Input:
```yaml
textbook2:
  type: book
  title: 'Лингвистика речевого воздействия и манипулирования'
  note: 'Учебное пособие'
  author:
    - {name: Балахонская, given-name: 'Л В'}
    - {name: Сергеева, given-name: 'Е В'}
  location: Нижний Новгород
  publisher: 'ФЛИНТА: Наука'
  date: 2016
  page-range: 111-135
  isbn: 78-5-94622-775-9
```
Output:

`Балахонская, Л. В. Лингвистика речевого воздействия и манипулирования : Учебное пособие / Л. В. Балахонская, Е. В. Сергеева. - Нижний Новгород: ФЛИНТА: Наука, 2016. - С. 111-135. - ISBN 78-5-94622-775-9.`

Input:
```yaml
textbook3:
  type: book
  title: 'Сенсорная экология'
  note: 'Учебное пособие для вузов'
  author:
    - {name: Дмитриева, given-name: 'Т М'}
    - {name: Козлов, given-name: 'Ю П'}
  editor:
    - { name: Иванов, given-name: 'И И' }
    - { name: Петров, given-name: 'А В' }
    - { name: Готкин, given-name: 'А М' }
  affiliated:
    - { role: compiler, names: ['Сергеев, А Г', 'Петров, А И'] }
    - { role: translator, names: ['Перлов, А Ю'] }
  edition: '2-е изд., перераб. и доп.; Юбилейное издание'
  location: Москва
  publisher: РУДН
  date: 2010
  page-range: 404
```
Output:

`Дмитриева, Т. М. Сенсорная экология : Учебное пособие для вузов / Т. М. Дмитриева, Ю. П. Козлов; ред. И. И. Иванов, А. В. Петров и А. М. Готкин; составит. А. Г. Сергеев и А. И. Петров; перевод. А. Ю. Перлов. - 2-е изд., перераб. и доп.; Юбилейное издание. - М.: РУДН, 2010. - 404 с.`

Input:
```yaml
book1:
  type: book
  author:
    - {name: Абдрахимов, given-name: 'В З'}
    - {name: Кайракбаев, given-name: 'А К'}
    - {name: Абдрахимова, given-name: 'Е С'}
  editor:
    - { name: Иванов, given-name: 'И И' }
    - { name: Петров, given-name: 'А В' }
  title: 'Экологические и практические аспекты использования отходов цветной металлургии в производстве кислотоупоров и плиток для полов'
  location: Актобе
  publisher: РИО Учреждения Актюбинский университет им. академика С. Баишева
  date: 2018
  page-range: 200
  isbn: 78-601-7566-37-1
```
Output:

`Абдрахимов, В. З. Экологические и практические аспекты использования отходов цветной металлургии в производстве кислотоупоров и плиток для полов / В. З. Абдрахимов, А. К. Кайракбаев, Е. С. Абдрахимова; ред. И. И. Иванов и А. В. Петров. - Актобе: РИО Учреждения Актюбинский университет им. академика С. Баишева, 2018. - 200 с. - ISBN 78-601-7566-37-1.`

Input:
```yaml
book2:
  type: book
  author:
    - {name: Багдасарян, given-name: 'В Э'}
    - {name: Орлов, given-name: 'И Б'}
    - {name: Катагошина, given-name: 'М В'}
    - {name: Коротков, given-name: 'С А'}
  editor:
    - {name: Иванов, given-name: 'И И'}
  title: 'История сервиса'
  note: 'учебное пособие'
  location: Москва
  edition: 2-е изд. перераб. и доп
  publisher: ИНФРА-М
  date: 2018
  page-range: 337
  isbn: 978-5-16-012845-0
```
Output:

`История сервиса : учебное пособие / В. Э. Багдасарян, И. Б. Орлов, М. В. Катагошина, С. А. Коротков; ред. И. И. Иванов. - 2-е изд. перераб. и доп. - М.: ИНФРА-М, 2018. - 337 с. - ISBN 978-5-16-012845-0.`

### Books with multiple volumes

Input:
```yaml
book3:
  type: book
  title: 'Курс дифференциального и интегрального исчисления'
  author:
    - {name: Фихтенгольц, given-name: 'Г М'}
  volume: 2
  volume-total: 3
  location: Москва
  publisher: Наука
  date: 2002
  page-range: 656
```
Output:

`Фихтенгольц, Г. М. Курс дифференциального и интегрального исчисления: в 3 т. Т. 2. / Г. М. Фихтенгольц. - М.: Наука, 2002. - 656 с.`

### Web resources

Input:
```yaml
web1:
  type: web
  title: 'Исследовано в России'
  note: 'многопредмет. науч. журн.'
  editor:
    - {name:  Власенко, given-name: 'Т В'}
  affiliated:
    - { role: 'Web-мастер', names: ['Козлова, Н В'] }
  location: Долгопрудный
  publisher: МФТИ
  edition: Электрон. журн.
  organization: Моск. физ.-техн. ин-т.
  date: 1998
  url: { value: http://zhurnal.mipt.rssi.ru, date: 2012-03-15}
```
Output:

`Исследовано в России [Электронный ресурс] : многопредмет. науч. журн. / Моск. физ.-техн. ин-т.; ред. Т. В. Власенко; Web-мастер Н. В. Козлова. - Электрон. журн. - Долгопрудный: МФТИ, 1998. - URL: http://zhurnal.mipt.rssi.ru/ (дата обращения: 15.03.2012).`

### Articles from books

Input
```yaml
article1:
  type: article
  title: 'Co-clustering documents and words using bipartite spectral graph partitioning'
  author:
    - { name: 'Dhillon', given-name: 'I' }
  page-range: 269-274
  parent:
    type: book
    title: Proceedings of the Seventh ACM SIGKDD International Conference on Knowledge Discovery and Data Mining
    location: New York, NY, USA
    publisher: ACM
    date: 2001
```

Output:
`Dhillon, I. Co-clustering documents and words using bipartite spectral graph partitioning / I. Dhillon // Proceedings of the Seventh ACM SIGKDD International Conference on Knowledge Discovery and Data Mining. - New York, NY, USA: ACM, 2001. - С. 269-274.`

### Articles from journals

Input:
```yaml
article2:
  type: article
  title: 'Использование мер релевантности строка-текст для автоматизации рубрикации научных статей'
  author:
    - { name: 'Миркин', given-name: 'Б Г' }
    - { name: 'Черняк', given-name: 'Е Л' }
  page-range: 51-62
  parent:
    type: periodical
    title: Бизнес-информатика
    date: 2014
    issue: №2 (28)
```

Output:

`Миркин, Б. Г. Использование мер релевантности строка-текст для автоматизации рубрикации научных статей / Б. Г. Миркин, Е. Л. Черняк // Бизнес-информатика. - 2014. - №2 (28). - С. 51-62.`

### PhD Thesis (dissertation)

Input:
```yaml
thesis1:
  type: thesis
  title: 'Религиозная политика Золотой Орды на Руси в XIII–XIV вв.'
  note: 'дис. ... канд. ист. наук : 07.00.02: защищена 22.01.02: утв. 15.07.02 '
  author:
    - { name: 'Белозеров', given-name: 'И В' }
  page-range: 215
  location: Москва
  date: 2002
```

Output:

`Белозеров, И. В. Религиозная политика Золотой Орды на Руси в XIII–XIV вв. : дис. ... канд. ист. наук : 07.00.02: защищена 22.01.02: утв. 15.07.02 / И. В. Белозеров. - М., 2002. - 215 с.`

### Thesis Abstract (AutoAbstract)

Input:
```yaml
thesis2:
  type: thesis
  title: 'Коммуникативная направленность текстов политической рекламы'
  note: 'автореф. дис. ... канд. филолог. наук: 10.02.01: защищена 16.12.05'
  author:
    - { name: 'Столярова', given-name: 'Е В' }
  organization: Поморский государственный университет им. М.В. Ломоносова
  page-range: 22
  location: Архангельск
  date: 2005
```

Output:
`Столярова, Е. В. Коммуникативная направленность текстов политической рекламы : автореф. дис. ... канд. филолог. наук: 10.02.01: защищена 16.12.05 / Е. В. Столярова; Поморский государственный университет им. М.В. Ломоносова. - Архангельск, 2005. - 22 с.`
