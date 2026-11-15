/* Official legal excerpts; Japanese translations and broad IPA are editorial study aids.
   Scope, pronunciation conventions, source and edition are recorded for each country. */
window.CONSTITUTIONS_DATA = {
  verifiedOn: '2026-11-15',
  countries: [
    {
      id: 'jp', name: '日本', englishName: 'Japan', code: 'JP', language: '日本語', lang: 'ja',
      documentTitle: '日本国憲法',
      pronunciationNote: '標準的な日本語の広いIPA転写。長音を示し、高低アクセント・母音の無声化などの細部は省略しています。',
      contextNote: '日本語が原文です。「日本語」欄にも同じ原文を表示します。条文全体ではなく、明示した範囲の抜粋です。',
      sources: [{label: '衆議院｜日本国憲法全文', url: 'https://www.shugiin.go.jp/internet/itdb_annai.nsf/html/statics/shiryo/dl-constitution.htm'}],
      excerpts: [
        {id: 'jp-13', article: '第13条・第1文', title: '個人の尊重', themes: ['尊厳'],
          original: 'すべて国民は、個人として尊重される。',
          translation: 'すべて国民は、個人として尊重される。',
          reading: 'すべて こくみんは、こじんとして そんちょうされる。',
          ipa: '[sɯbete kokɯmiɴ wa koʑiɴ toɕite soɴtɕoː saɾeɾɯ]',
          scopeNote: '第13条の第1文。生命・自由・幸福追求と公共の福祉についての後続文は、出典全文で読めます。',
          sourceUrl: 'https://www.shugiin.go.jp/internet/itdb_annai.nsf/html/statics/shiryo/dl-constitution.htm'},
        {id: 'jp-21', article: '第21条・第1項', title: '表現の自由', themes: ['自由'],
          original: '集会、結社及び言論、出版その他一切の表現の自由は、これを保障する。',
          translation: '集会、結社及び言論、出版その他一切の表現の自由は、これを保障する。',
          reading: 'しゅうかい、けっしゃ および げんろん、しゅっぱん そのた いっさいの ひょうげんの じゆうは、これを ほしょうする。',
          ipa: '[ɕɯːkai keɕːa ojobi ɡeɴɾoɴ ɕɯpːaɴ sonota isːai no çoːɡeɴ no dʑijɯː wa koɾe o hoɕoː sɯɾɯ]',
          scopeNote: '第21条の第1項。第2項の検閲・通信の秘密に関する規定は含みません。',
          sourceUrl: 'https://www.shugiin.go.jp/internet/itdb_annai.nsf/html/statics/shiryo/dl-constitution.htm'}
      ]
    },
    {
      id: 'us', name: '米国', englishName: 'United States', code: 'US', language: '英語', lang: 'en-US',
      documentTitle: 'Constitution of the United States',
      pronunciationNote: '現代の一般米語を想定した広いIPA転写。強勢を示し、文全体のイントネーションや細かな連結音は省略しています。',
      contextNote: '米国国立公文書館の原綴り・大文字表記に従います。本文と、1791年に批准された修正第8条から各1件を選びました。',
      sources: [
        {label: 'National Archives｜憲法本文', url: 'https://www.archives.gov/founding-docs/constitution-transcript'},
        {label: 'National Archives｜権利章典（修正第1〜10条）', url: 'https://www.archives.gov/founding-docs/bill-of-rights-transcript'}
      ],
      excerpts: [
        {id: 'us-i-1', article: 'Article I, Section 1', title: '連邦議会と立法権', themes: ['統治'],
          original: 'All legislative Powers herein granted shall be vested in a Congress of the United States, which shall consist of a Senate and House of Representatives.',
          translation: 'この憲法によって与えられるすべての立法権は、上院と下院から成る合衆国議会に属する。',
          ipa: '[ɔl ˈlɛdʒəˌsleɪtɪv ˈpaʊɚz ˌhɪɹˈɪn ˈɡɹæntɪd ʃəl bi ˈvɛstɪd ɪn ə ˈkɑŋɡɹəs əv ðə juˈnaɪtɪd steɪts wɪtʃ ʃəl kənˈsɪst əv ə ˈsɛnət ənd haʊs əv ˌɹɛpɹɪˈzɛntətɪvz]',
          scopeNote: '第1条第1節の全文。herein は「この憲法によって」と訳しています。',
          sourceUrl: 'https://www.archives.gov/founding-docs/constitution-transcript'},
        {id: 'us-viii', article: 'Amendment VIII', title: '過大な保釈金・罰金と刑罰', themes: ['尊厳'],
          original: 'Excessive bail shall not be required, nor excessive fines imposed, nor cruel and unusual punishments inflicted.',
          translation: '過大な保釈金を要求し、過大な罰金を科し、また残虐で異常な刑罰を科してはならない。',
          ipa: '[ɪkˈsɛsɪv beɪl ʃəl nɑt bi ɹɪˈkwaɪɚd nɔɹ ɪkˈsɛsɪv faɪnz ɪmˈpoʊzd nɔɹ ˈkɹuəl ənd ʌnˈjuʒuəl ˈpʌnɪʃmənts ɪnˈflɪktɪd]',
          scopeNote: '修正第8条の全文。英国の権利章典の文言とも読み比べられます。',
          sourceUrl: 'https://www.archives.gov/founding-docs/bill-of-rights-transcript'}
      ]
    },
    {
      id: 'gb', name: '英国', englishName: 'United Kingdom', code: 'GB', language: '英語', lang: 'en-GB',
      documentTitle: '憲法関連文書：Bill of Rights（権利章典）',
      pronunciationNote: '原綴りは歴史的表記のまま、IPAは現代の標準的な英国英語による読みの目安です。1689年当時の発音の再現ではありません。',
      contextNote: '英国には単一の成文憲法典がありません。1689年成立の権利章典の原制定文から2節を選びました。法令サイトでの名称は「Bill of Rights [1688]」です。',
      sources: [
        {label: 'legislation.gov.uk｜Bill of Rights [1688] 原制定文', url: 'https://www.legislation.gov.uk/aep/WillandMarSess2/1/2/enacted'},
        {label: 'UK Parliament｜権利章典の成立', url: 'https://www.parliament.uk/about/living-heritage/evolutionofparliament/parliamentaryauthority/revolution/overview/billofrights/'},
        {label: 'UK Parliament｜英国の憲法', url: 'https://www.parliament.uk/about/how/role/sovereignty/'}
      ],
      excerpts: [
        {id: 'gb-election', article: 'Freedom of Election の節', title: '議員選挙の自由', themes: ['自由', '統治'],
          original: 'That Election of Members of Parlyament ought to be free.',
          translation: '議会議員の選挙は自由でなければならない。',
          ipa: '[ðæt ɪˈlɛkʃən əv ˈmɛmbəz əv ˈpɑːləmənt ɔːt tə biː fɹiː]',
          scopeNote: '「Freedom of Election」の節の全文。「That」は宣言事項を列挙する文脈から続く語です。Parlyament は原綴り。',
          sourceUrl: 'https://www.legislation.gov.uk/aep/WillandMarSess2/1/2/enacted'},
        {id: 'gb-bail', article: 'Excessive Bail の節', title: '過大な保釈金・罰金と刑罰', themes: ['尊厳'],
          original: 'That excessive Baile ought not to be required nor excessive Fines imposed nor cruell and unusuall Punishments inflicted.',
          translation: '過大な保釈金を要求し、過大な罰金を科し、また残虐で異常な刑罰を科してはならない。',
          ipa: '[ðæt ɪkˈsɛsɪv beɪl ɔːt nɒt tə biː ɹɪˈkwaɪəd nɔː ɪkˈsɛsɪv faɪnz ɪmˈpəʊzd nɔː ˈkɹuːəl ənd ʌnˈjuːʒuəl ˈpʌnɪʃmənts ɪnˈflɪktɪd]',
          scopeNote: '「Excessive Bail」の節の全文。Baile / cruell / unusuall は原制定文の綴りです。',
          sourceUrl: 'https://www.legislation.gov.uk/aep/WillandMarSess2/1/2/enacted'}
      ]
    },
    {
      id: 'fr', name: 'フランス', englishName: 'France', code: 'FR', language: 'フランス語', lang: 'fr',
      documentTitle: 'Constitution du 4 octobre 1958',
      pronunciationNote: 'フランス本土の標準的な発音を想定した広いIPA転写。鼻母音と必要なリエゾンを示し、文の抑揚は省略しています。',
      contextNote: '1958年憲法から国の性格と標語を選びました。共和国の標語は自由・平等・友愛です。',
      sources: [{label: 'Légifrance｜1958年憲法全文', url: 'https://www.legifrance.gouv.fr/loda/texte_lc/LEGITEXT000006071194/'}],
      excerpts: [
        {id: 'fr-1', article: 'Article 1er・第1文', title: '共和国の性格', themes: ['統治', '主権'],
          original: 'La France est une République indivisible, laïque, démocratique et sociale.',
          translation: 'フランスは、不可分で、非宗教的で、民主的かつ社会的な共和国である。',
          ipa: '[la fʁɑ̃s ɛt yn ʁepyblik ɛ̃divizibl la.ik demɔkʁatik e sɔsjal]',
          scopeNote: '第1条の第1文。laïque は国家の宗教的中立性に関わる語で、ここでは「非宗教的」と訳しています。',
          sourceUrl: 'https://www.legifrance.gouv.fr/loda/texte_lc/LEGITEXT000006071194/'},
        {id: 'fr-2', article: 'Article 2・標語の文', title: '自由・平等・友愛', themes: ['自由', '平等'],
          original: 'La devise de la République est " Liberté, Egalité, Fraternité ".',
          translation: '共和国の標語は「自由、平等、友愛」である。',
          ipa: '[la dəviz də la ʁepyblik ɛ libɛʁte eɡalite fʁatɛʁnite]',
          scopeNote: '第2条のうち標語を定める1文を抜粋。引用符・Egalité の綴りはLégifranceの掲載表記に従います。',
          sourceUrl: 'https://www.legifrance.gouv.fr/codes/article_lc/LEGIARTI000006527453'}
      ]
    },
    {
      id: 'de', name: 'ドイツ', englishName: 'Germany', code: 'DE', language: 'ドイツ語', lang: 'de',
      documentTitle: 'Grundgesetz für die Bundesrepublik Deutschland（基本法）',
      pronunciationNote: 'ドイツの標準語を想定した広いIPA転写。語の強勢・母音の長短を示します。rの発音には地域差があります。',
      contextNote: '憲法にあたる「基本法」から、人間の尊厳と法の前の平等についての短い規定を選びました。',
      sources: [{label: 'Gesetze im Internet｜基本法全文', url: 'https://www.gesetze-im-internet.de/gg/BJNR000010949.html'}],
      excerpts: [
        {id: 'de-1', article: 'Artikel 1 Absatz 1・第1文', title: '人間の尊厳', themes: ['尊厳'],
          original: 'Die Würde des Menschen ist unantastbar.',
          translation: '人間の尊厳は、侵してはならない。',
          ipa: '[diː ˈvʏʁdə dɛs ˈmɛnʃən ɪst ʊnʔanˈtastbaːɐ̯]',
          scopeNote: '第1条第1項の第1文。尊厳を尊重・保護する国家権力の義務についての後続文は含みません。',
          sourceUrl: 'https://www.gesetze-im-internet.de/gg/art_1.html'},
        {id: 'de-3', article: 'Artikel 3 Absatz 1', title: '法の前の平等', themes: ['平等'],
          original: 'Alle Menschen sind vor dem Gesetz gleich.',
          translation: 'すべての人は、法の前に平等である。',
          ipa: '[ˈalə ˈmɛnʃən zɪnt foːɐ̯ deːm ɡəˈzɛts ɡlaɪç]',
          scopeNote: '第3条第1項の全文。第2項・第3項の平等や差別禁止に関する規定は含みません。',
          sourceUrl: 'https://www.gesetze-im-internet.de/gg/art_3.html'}
      ]
    },
    {
      id: 'cn', name: '中国', englishName: 'China', code: 'CN', language: '中国語（普通話）', lang: 'zh-Hans',
      documentTitle: '中华人民共和国宪法',
      pronunciationNote: '普通話。ピンインとIPAは語の基本声調を表示します。˥は高い声調、˧˥は上昇、˨˩˦は低く下がって上昇、˥˩は下降。連続発話では「一切」がyíqièになるなど変調します。',
      contextNote: '外交部掲載の2018年修正を含む原文から抜粋。テーマによる文言の比較であり、各国の制度や権利の保障範囲が同じという意味ではありません。',
      sources: [{label: '中国外交部｜中華人民共和国憲法全文（2018年修正を収録）', url: 'https://www.mfa.gov.cn/ziliao_674904/zt_674979/dnzt_674981/qtzt/twwt/stflgf/202206/t20220606_10698959.html'}],
      excerpts: [
        {id: 'cn-2', article: '第二条・第1段落', title: '国家権力と人民', themes: ['主権', '統治'],
          original: '中华人民共和国的一切权力属于人民。',
          translation: '中華人民共和国のすべての権力は、人民に属する。',
          pinyin: 'Zhōnghuá Rénmín Gònghéguó de yīqiè quánlì shǔyú rénmín.',
          ipa: '[ʈʂʊŋ˥ xwa˧˥ ʐən˧˥ min˧˥ kʊŋ˥˩ xɤ˧˥ ku̯ɔ˧˥ tə i˥ tɕʰjɛ˥˩ tɕʰɥɛn˧˥ li˥˩ ʂu˨˩˦ y˧˥ ʐən˧˥ min˧˥]',
          scopeNote: '第2条の第1段落。人民代表大会など、権力行使の仕組みについての後続段落は含みません。',
          sourceUrl: 'https://www.mfa.gov.cn/ziliao_674904/zt_674979/dnzt_674981/qtzt/twwt/stflgf/202206/t20220606_10698959.html'},
        {id: 'cn-33', article: '第三十三条・第3段落', title: '人権の尊重と保障', themes: ['尊厳'],
          original: '国家尊重和保障人权。',
          translation: '国家は、人権を尊重し、保障する。',
          pinyin: 'Guójiā zūnzhòng hé bǎozhàng rénquán.',
          ipa: '[ku̯ɔ˧˥ tɕja˥ tswən˥ ʈʂʊŋ˥˩ xɤ˧˥ pau̯˨˩˦ ʈʂɑŋ˥˩ ʐən˧˥ tɕʰɥɛn˧˥]',
          scopeNote: '第33条の第3段落のみ。国籍・法の前の平等・公民の義務についての他の段落は含みません。',
          sourceUrl: 'https://www.mfa.gov.cn/ziliao_674904/zt_674979/dnzt_674981/qtzt/twwt/stflgf/202206/t20220606_10698959.html'}
      ]
    },
    {
      id: 'kr', name: '韓国', englishName: 'South Korea', code: 'KR', language: '韓国語', lang: 'ko',
      documentTitle: '대한민국헌법（大韓民国憲法）',
      pronunciationNote: 'ソウル標準語を想定した広いIPA転写。鼻音化・連音を一部反映し、語境界での細かな有声化と母音の長短は省略。属格의には許容される[e]の読みを用います。',
      contextNote: '国家法令情報センター掲載の韓国語原文を使用。掲載版には1987年10月29日全部改正・1988年2月25日施行と記載されています。',
      sources: [{label: '国家法令情報センター｜大韓民国憲法・第10〜22条', url: 'https://www.law.go.kr/lsLinkCommonInfo.do?chrClsCd=010202&lsJoLnkSeq=1026032837'}, {label: '国家法令情報センター｜大韓民国憲法全文', url: 'https://www.law.go.kr/법령/대한민국헌법'}],
      excerpts: [
        {id: 'kr-11', article: '제11조 제1항・第1文', title: '法の前の平等', themes: ['平等'],
          original: '모든 국민은 법 앞에 평등하다.',
          translation: 'すべての国民は、法の前に平等である。',
          ipa: '[mo.dɯn kuŋ.mi.nɯn pʌ.ba.pʰe pʰjʌŋ.dɯŋ.ha.da]',
          scopeNote: '第11条第1項の第1文。性別・宗教・社会的身分などによる差別禁止を定める後続文は含みません。',
          sourceUrl: 'https://www.law.go.kr/lsLinkCommonInfo.do?chrClsCd=010202&lsJoLnkSeq=1026032837'},
        {id: 'kr-19', article: '제19조', title: '良心の自由', themes: ['自由'],
          original: '모든 국민은 양심의 자유를 가진다.',
          translation: 'すべての国民は、良心の自由を有する。',
          ipa: '[mo.dɯn kuŋ.mi.nɯn jaŋ.ɕi.me tɕa.ju.ɾɯl ka.dʑin.da]',
          scopeNote: '第19条の全文。原文の양심は「良心」です。',
          sourceUrl: 'https://www.law.go.kr/lsLinkCommonInfo.do?chrClsCd=010202&lsJoLnkSeq=1026032837'}
      ]
    },
    {
      id: 'in', name: 'インド', englishName: 'India', code: 'IN', language: '英語', lang: 'en',
      documentTitle: 'The Constitution of India（英語原文）',
      pronunciationNote: '英語の原文を一般米語で読んだ場合の広いIPA転写。インド英語固有の発音を表すものではありません。Bharatも英語化した読みの目安です。',
      contextNote: '立法局公開の政府刊行版（2025年11月11日現在と表記された英語・カンナダ語対訳版）の英語本文を使用。ヒンディー語の翻字ではありません。',
      sources: [{label: 'インド法務・司法省 立法局｜政府刊行憲法PDF（2025年11月11日現在）', url: 'https://www.legislative.gov.in/static/uploads/2025/07/359f70a69695affb9d72f8393102bd2e.pdf'}],
      excerpts: [
        {id: 'in-1', article: 'Article 1(1)', title: '連邦の名称と構成', themes: ['統治'],
          original: 'India, that is Bharat, shall be a Union of States.',
          translation: 'インディア、すなわちバーラトは、諸州の連邦とする。',
          ipa: '[ˈɪndiə ðæt ɪz ˈbɑɹət ʃəl bi ə ˈjunjən əv steɪts]',
          scopeNote: '第1条第1項の全文。州・領域を定める第2項以降は含みません。',
          sourceUrl: 'https://www.legislative.gov.in/static/uploads/2025/07/359f70a69695affb9d72f8393102bd2e.pdf'},
        {id: 'in-14', article: 'Article 14', title: '法の前の平等と平等な保護', themes: ['平等'],
          original: 'The State shall not deny to any person equality before the law or the equal protection of the laws within the territory of India.',
          translation: '国家は、インドの領域内において、いかなる人に対しても、法の前の平等又は法による平等な保護を否定してはならない。',
          ipa: '[ðə steɪt ʃəl nɑt dɪˈnaɪ tə ˈɛni ˈpɝsən ɪˈkwɑləti bɪˈfɔɹ ðə lɔ ɔɹ ði ˈikwəl pɹəˈtɛkʃən əv ðə lɔz wɪˈðɪn ðə ˈtɛɹəˌtɔɹi əv ˈɪndiə]',
          scopeNote: '第14条の全文。主体は「国民」に限定されず、原文どおり「いかなる人」と訳しています。',
          sourceUrl: 'https://www.legislative.gov.in/static/uploads/2025/07/359f70a69695affb9d72f8393102bd2e.pdf'}
      ]
    }
  ]
};
