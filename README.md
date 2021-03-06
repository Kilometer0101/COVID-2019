---
title: "References"
author: "km"
date: "2020/05/15"
output: 
  html_document:
    keep_md: true
---



## Data from JP Ministry

[報道発表一覧(厚労省)](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/0000121431_00086.html)

![](./fig/fig_Jp.png)

[code](Conf_Death_jp.md)

[data.csv](data/corona_conf_death_jp.csv)

## Data from Tokyo Prefecture

[東京都コロナウィルス感染症対策サイト](https://stopcovid19.metro.tokyo.lg.jp/)

![](./fig/fig_Tokyo.png)

![](./fig/fig_Tokyo_log.png)



[code](Tokyo.md)

[data](data/130001_tokyo_covid19_patients.csv)

## Refs.

[Coronavirus disease (COVID-2019) situation reports (WHO)](https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports)


[COVID-19重症者における人工呼吸器装着数の推移](https://covidecmo.txpmedical.com/)

[COVID-19 testing](https://en.wikipedia.org/wiki/COVID-19_testing)


[問14　集団感染を防ぐためにはどうすればよいでしょうか？](https://www.mhlw.go.jp/stf/seisakunitsuite/bunya/kenkou_iryou/dengue_fever_qa_00001.html#Q14)


[クラスター対応戦略の概要（2020年3月10日暫定版）](https://www.jsph.jp/files/docments/COVID-19_031102.pdf)

[山中伸弥による新型コロナウイルス情報発信](https://www.covid19-yamanaka.com/index.html)

### [Lai, C. et al., "Severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) and coronavirus disease-2019 (COVID-19): The epidemic and the challenges", Int. J. of Antimicrobial Agents, 12 Feb 2020](https://www.sciencedirect.com/science/article/pii/S0924857920300674)

> 8. Infection control and prevention  
> ... Experience from the early phase of SARS-CoV-2 pneumonia strongly highlighted that travel history, rather than chest radiography, is of paramount importance for early detection and isolation of SARS-CoV-2 pneumonia cases [41].

SARS-CoV-2肺炎の初期段階での経験から、SARS-CoV-2肺炎症例の早期発見と分離のためには、胸部X線撮影よりも渡航歴が最も重要であることが強く強調されている[Kim et al., J Korean Med Sci, 35 (03 Feb 2020)](https://synapse.koreamed.org/DOIx.php?id=10.3346/jkms.2020.35.e61)。


### [Hung, C. et al., "Clinical features of patients infected with 2019 novel coronavirus in Wuhan, China", Lancet, 24 Jan 2020](https://www.thelancet.com/action/showPdf?pii=S0140-6736%2820%2930183-5)

> By  Jan 2, 2020, 41 admitted hospital patients had been identified as having laboratory-confirmed 2019-nCoVinfection.

2020年1月2日までに、41人の入院患者が検査室で確認された2019-nCoV感染症を有していることが確認された。

> 27 (66%) of 41 patients had a history of direct exposure to the Huanan seafood market. 

41人のうち27例(66%)では武漢華南海鮮卸売市場における直接的な暴露履歴があった。

> In this  cohort,  most  patients  presented  with  fever,  dry  cough, dyspnoea, and bilateral ground-glass opacities on chest  CT  scans.

このコホートでは、大部分の患者が熱・乾性咳・呼吸困難・およびCTスキャン像における両側スリガラス陰影が認められた。

> Our  study  has  some  limitations. ...This  is  a  modest-sized  case  series  of  patients  admitted  to  hospital;  collection  of  standardised  data  for  a  larger  cohort  would  help  to  further  define  the  clinical  presentation,  natural  history,  and  risk  factors. 

本研究にはいくつかの限界がある。...これは入院患者群の小規模な集団を対象としている。より大規模なコホートの標準化されたデータを集計することで、臨床的な特徴・自然経過・リスク要因についてさらなる解明に役立つだろう。

※ PCR検査の詳細

>  The presence of 2019-nCoV in respiratory specimens was detected by next-generation sequencing or real-time RT-PCR methods. The primers  and  probe  target  to  envelope  gene  of  CoV  were  used  and  the  sequences  were  as  follows:  forward  primer  5′-ACTTCTTTTTCTTGCTTTCGTGGT-3′; reverse primer 5′-GCAGCAGTACGCACACAATC-3′;    and    the    probe    5′CY5-CTAGTTACACTAGCCATCCTTACTGC-3′BHQ1. 

### [Chen, N. et al., "Epidemiological and clinical characteristics of 99 cases of 2019 novel coronavirus pneumonia in Wuhan, China: a descriptive study", Lancet, 30 Jan 2020](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30211-7/fulltext)

> we included all confirmed cases of 2019-nCoV in Wuhan Jinyintan Hospital from Jan 1 to Jan 20, 2020. 

武漢金仁丹病院で 1 月 1 日から 2020 年 1 月 20 日までに確認された 2019-nCoV の全症例を対象とした。

> 99 patients with 2019-nCoV were included in this study, two of whom were husband and wife. In total, 49 (49%) patients were clustered and had a history of exposure to the Huanan seafood market. 

99人の2019-nCoV患者がこの研究の対象に含まれている。2組みの夫婦が含まれている。全例のうち49例(49%)は黄南海鮮市場での暴露歴を持っており**クラスタ化していた**。

> According to chest x-ray and CT, 74 (75%) patients showed bilateral pneumonia (75%) with just 25 (25%) patients showing unilateral pneumonia (table 2). 14 (14%) patients showed multiple mottling and ground-glass opacity (table 2; figure). Additionally, pneumothorax occurred in one (1%) patient.

X線・CTスキャン像では74例(75%)の患者において両側性肺炎が認められ、片側性肺炎は14例(14%)であった。14例(14%)の患者では複数オン斑状の浸潤影とスリガラス陰影が認められた。これに加え、1例(1%)では気胸が発生した。

### [Wang, D. et al., "Clinical Characteristics of 138 Hospitalized Patients With 2019 Novel Coronavirus–Infected Pneumonia in Wuhan, China", JAMA, 7 Feb 2020](https://jamanetwork.com/journals/jama/fullarticle/2761044)

>  In this single-center case series involving 138 patients with NCIP, 26% of patients required admission to the intensive care unit and 4.3% died.

この症例群には単施設(武漢大学中南病院)における患者138人が含まれ、26％の患者が集中治療室への入院を必要とし、4.3％が死亡した。

> Hospital-associated transmission was suspected as the presumed mechanism of infection for affected health professionals (40 [29%]) and hospitalized patients (17 [12.3%]).

医療従事者40例(29%)と入院患者17例(12.3%)について推定される感染経路は院内感染が疑われる。

> Huang et al first reported 41 cases of NCIP in which most patients had a history of exposure to Huanan Seafood Wholesale Market. ...Subsequently, Chen et al reported findings from 99 cases of NCIP from the same hospital and the results suggested that the 2019-nCoV infection clustered within groups of humans in close contact, was more likely to affect older men with comorbidities, and could result in ARDS

Huangらの最初の41例の報告ではほとんどの患者が黄南海鮮卸売市場に暴露された経験を持っていた。その後のChenらの99例の単一病院における報告では、2019-nCoV感染は濃厚接触したグループにおいて**クラスタ化**されており、併存疾患を持つ高齢男性に対して影響を与えやすく、それがARDS(急性呼吸窮迫症候群)を引き起こす可能性があるとされる。

> The data in this study suggest rapid person-to-person transmission of 2019-nCoV may have occurred. The main reason is derived from the estimation of the basic reproductive number (R0) based on a previous study [15]. ... Based on the report, R0 from nCoV is 2.2, which estimated that, on average, each patient has been spreading infection to 2.2 other people[15].

この研究のデータからは、ヒトからヒトへの急速な2019-nCoVの感染が生じた可能性が示唆される。主要な理由は先行研究において指摘された基本再生数(basic reproductive number, R0)に基づくと思われる。...この報告では、nCoVのR0は2.2と推定されている。この数値は、平均では、それぞれの患者は約2.2人に対して感染を広げることを意味する。 [Li Q et al., "Early transmission dynamics in Wuhan, China, of novel coronavirus-infected pneumonia", N Engl J Med, 29 Jan 2020](https://www.nejm.org/doi/10.1056/NEJMoa2001316)。

> Chest computed tomographic scans showed bilateral patchy shadows or ground glass opacity in the lungs of all patients.

CTスキャン像では全ての患者において両側肺の斑状影もしくはスリガラス陰影が認められた。

### [Li Q et al., "Early transmission dynamics in Wuhan, China, of novel coronavirus-infected pneumonia", N Engl J Med, 29 Jan 2020](https://www.nejm.org/doi/10.1056/NEJMoa2001316)

> The initial cases of novel coronavirus (2019-nCoV)–infected pneumonia (NCIP) occurred in Wuhan, Hubei Province, China, in December 2019 and January 2020. We analyzed data on the first 425 confirmed cases in Wuhan to determine the epidemiologic characteristics of NCIP.

2019年12月と2020年1月に中国湖北省武漢で発生したNCIP(新規コロナウイルス（2019-nCoV）感染性肺炎)の初期症例。武漢で最初に確認された425例のデータを分析し、NCIPの疫学的特徴を明らかにした。

> A suspected NCIP case was defined as a pneumonia that either fulfilled all the following four criteria — fever, with or without recorded temperature; radiographic evidence of pneumonia; low or normal white-cell count or low lymphocyte count; and no reduction in symptoms after antimicrobial treatment for 3 days, following standard clinical guidelines — or fulfilled the abovementioned first three criteria and had an epidemiologic link to the Huanan Seafood Wholesale Market or contact with other patients with similar symptoms.

NCIPが疑われる症例は、以下の4つの基準をすべて満たす肺炎と定義された：(1) 発熱（体温の履歴は有る場合と無い場合がある）、(2) 肺炎のX線写真的証拠、(3) 白血球数またはリンパ球数の低値または正常値、(4) 標準的な臨床ガイドラインに従った抗菌薬治療を3日間行っても症状が軽減しない。または上記の最初の3つの基準を満たし、かつ、黄南海鮮卸売市場との疫学的関連性を有するもしくは類似の症状を有する他の患者と接触した場合。

> We estimated the epidemic growth rate by analyzing data on the cases with illness onset between December 10 and January 4, ...

12 月 10 日から 1 月 4 日までの発症者のデータを解析し，流行の拡大率を推定した。

> We fitted a transmission model (formulated with the use of renewal equations) with zoonotic infections to onset dates that were not linked to the Huanan Seafood Wholesale Market, ...

人獣共通感染症の伝播モデル（更新方程式を用いて定式化したもの）を黄南水産物卸売市場に関連していない発症日に当てはめた。

> and we used this model to derive the epidemic growth rate, the epidemic doubling time, and the basic reproductive number (R0), which is defined as the expected number of additional cases that one case will generate, on average, over the course of its infectious period in an otherwise uninfected population. 

このモデルを用いて、流行拡大率、流行倍増時間、および基本再生数（R0）を導出した。これは、感染していない集団において、1つの症例が感染期間中に平均して発生する追加症例数として定義される。

> We fitted a gamma distribution to data from cluster investigations to estimate the serial interval distribution, defined as the delay between illness onset dates in successive cases in chains of transmission.

> We obtained information on 5 clusters of cases, shown in Figure 3. On the basis of the dates of illness onset of 6 pairs of cases in these clusters, we estimated that the serial interval distribution had a mean (±SD) of 7.5±3.4 days (95% CI, 5.3 to 19) (Figure 2B).

![](fig/fig_Li2020Fig3.png)

> Fig.3: Detailed Information on Exposures and Dates of Illness Onset in Five Clusters Including 16 Cases.

※ ここで「clusters」は、図の通り、連続的に感染が伝播したと認められる群という意味で使われていることに注意。

> In the epidemic curve up to January 4, 2020, the epidemic growth rate was 0.10 per day (95% CI, 0.050 to 0.16) and the doubling time was 7.4 days (95% CI, 4.2 to 14). Using the serial interval distribution above, we estimated that R0 was 2.2 (95% CI, 1.4 to 3.9).

>  Although the majority of the earliest cases were linked to the Huanan Seafood Wholesale Market and the patients could have been infected through zoonotic or environmental exposures, it is now clear that human-to-human transmission has been occurring and that the epidemic has been gradually growing in recent weeks. 

初期の症例の大部分は華南海鮮卸売市場との関連があり、患者は人獣共通感染症や環境曝露によって感染した可能性があった。しかし、現在では人から人への感染が発生していることが明らかになっており、ここ数週間で徐々に流行が拡大しつつある。

> Our estimate of R0 was limited to the period up to January 4 because increases in awareness of the outbreak and greater availability and use of tests in more recent weeks will have increased the proportions of infections ascertained. ... 

> ... it is now a priority to determine whether local transmission at a similar intensity is occurring in other locations.

現時点では、他の地域でも同様の強度で局所的な感染が発生していないかどうかを見極めることが優先されます。

## [Interim Clinical Guidance for Management of Patients with Confirmed Coronavirus Disease (COVID-19)](https://www.cdc.gov/coronavirus/2019-ncov/hcp/clinical-guidance-management-patients.html#foot04)

## [Articleswww.thelancet.com Vol 395   February 15, 2020507Epidemiological and clinical characteristics of 99 cases of 2019 novel coronavirus pneumonia in Wuhan, China: a descriptive study](https://www.thelancet.com/action/showPdf?pii=S0140-6736%2820%2930211-7)

## [Modeling the Transmission of Middle EastRespirator Syndrome Corona Virus in theRepublic of Korea](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4686901/pdf/pone.0144778.pdf)

## [The basic reproduction number of novel coronavirus (2019-nCoV) estimation based on exponential growth in the early outbreak in China from 2019 to 2020: A reply to Dhungana](https://reader.elsevier.com/reader/sd/pii/S1201971220300837?token=E0088C2AE5B95A03FA8B88A143E44660CE2D842BB49C509B1C6F3E369D8E8DFDF46303E3483A903D507B6917CCBD624E)

## [Characteristics of COVID-19 infection in Beijing](https://reader.elsevier.com/reader/sd/pii/S0163445320301018?token=59E09B8014DC57B092BD74B11A4954EAAC90C32453F436D2307B6A3DF7FB3934B3411492AE7C43A50528FEBEF40C0E0F)

## [Investigation of three clusters of COVID-19 in Singapore: implications for surveillance and response measures](https://www.thelancet.com/action/showPdf?pii=S0140-6736%2820%2930528-6)

## [Guidelines for Investigating Clusters of Health Events (CDC)](https://www.cdc.gov/mmwr/preview/mmwrhtml/00001797.htm)

## [WITHDRAWN: Experimental Treatment with Favipiravir for COVID-19: An Open-Label Control Study](https://www.sciencedirect.com/science/article/pii/S2095809920300631)

-> アビガンの臨床研究がWITHDRAW?（復活してた）

>  In this open-label before-after controlled study, FPV showed better therapeutic responses on COVID-19 in terms of disease progression and viral clearance. These preliminary clinical results provide useful information of treatments for SARS-CoV-2 infection.

## ["Closed environments facilitate secondary transmission of coronavirus disease 2019 (COVID-19)", Nishiura H. et al., medRxiv, 03 Mar](https://www.medrxiv.org/content/10.1101/2020.02.28.20029272v1)

> As of 26 February 2020, we examined a total of 110 cases among eleven clusters and investigated who acquired infection from whom. The clusters included four in Tokyo and one each in Aichi, Fukuoka, Hokkaido, Ishikawa, Kanagawa and Wakayama prefectures. 

2月26日時点で、11のクラスターに渡る110のケースを分析し、誰が誰から感染したのかを調査した。そのクラスタには東京の4例、愛知・福岡・北海道・石川・神奈川・和歌山のそれぞれ1例が含まれていた。

> All clusters were associated with close contact in indoor environments, including fitness gyms, a restaurant boat on a river, hospitals, and a snow festival where there were eating spaces in tents with minimal ventilation rate.

全てのクラスターは室内環境 (indoor environments) 中の濃厚接触 (close contact) に関連づけられていた。例えば、フィットネスジム・屋形船のレストラン・病院・通気性のよくない食事スペースのある雪まつりなどが含まれていた。

> The number of secondary cases generated by each primary case was calculated using contact tracing data. 

それぞれの一次感染者による二次感染数は接触追跡データ (contact tracing data) から算出した。

>  Of the 110 cases examined, 27 (24.6%) were primary cases who generated secondary cases. Figure 1 shows the distribution of these transmissions of which the mean and variance were 0.6 cases and 2.5 cases2, respectively. The odds that a primary case transmitted COVID-19 in a closed environment was 18.7 times greater compared to an open-air environment


110ケースにおいて、27例 (24.6%) は2次感染を生み出した一次感染者であった。図1はこれらの伝播の分布を示す...。一次感染者が閉鎖環境でCOVID-19を伝播したケースは、開放環境 (open-air env.) に比べて18.7倍であった。

![](./fig/fig_Nishiura2020.png)

> If superspreading events are defined as events where the number of secondary cases generated by a single primary case is greater than the 95th percentile of the　distribution (i.e. transmission to three or more persons), then eleven of the 110 cases (10.0%) were involved in such events.

超拡散事例 (superspreading events) を「一次感染者によって生じた2次感染が95%タイルよりも多い(すなわち3人以上であった)ケース」と定義すると、110ケースのうち11例 (10%) がこれに含まれる。

>  Nine of these events (81.8%) took place in closed environments, and the odds ratio (OR) of superspreading events in closed environments was as high as 29.8 (95% CI: 5.8, 153.4).

このイベントのうち9例 (81.8%) が閉鎖環境において発生し、閉鎖環境における超拡散事例のオッズ比は29.8と高い数値を示した。

> It is plausible that closed environments contribute to secondary transmission of COVID-19 and promote superspreading events. 

閉鎖環境がCOVID-19の二次感染に寄与し、超拡散事例を促進している可能性がある。

[この論文に関するコメントを別にまとめました。](https://note.com/kilometer/n/nf457860c3416)

## ["SARS-CoV-2 detection in patients with influenza-like illness", Kong, WH et al., NatMicrobio 07 April 2020](https://www.nature.com/articles/s41564-020-0713-1)

## ["No Association of COVID-19 transmission with temperature or UV radiation in Chinese cities", Yao et al., 29 Mar 2020](https://erj.ersjournals.com/content/early/2020/04/01/13993003.00517-2020)


## ["Chest Computed Tomography for Detection of Coronavirus Disease 2019 (COVID-19): Don't Rush the Science", Hope et al., AoIM, 08 April](https://annals.org/aim/fullarticle/2764546/chest-computed-tomography-detection-coronavirus-disease-2019-covid-19-don)

>  Amidst the coronavirus disease 2019 (COVID-19) pandemic, there is great pressure on physicians to provide clarity and answers. Good science, however, takes time and careful consideration to prove the value of advancements in diagnosis and treatment.

コロナウィルス疾患(COVID-19)が大流行する中、医師には明快さと解決策を求める非常に高いプレッシャーが掛かっている。しかしながら、優れた科学には時間がかかり、また診断と治療における進歩の価値について注意深い検討が必要です。

> A rush to publish positive results leads to their overinterpretation and, consequently, the dissemination of premature conclusions with broad implications.

肯定的な結果の出版を急ぐと過剰な解釈が引き起こされ、結果的に未熟な結論が広い範囲に普及してしまう。

> The findings of COVID-19 pneumonia that were used (for example, consolidation and ground-glass opacity) are not specific to the disease; rather, they are commonly seen in a range of infectious and noninfectious conditions. Consequently, positive CT results are only believable if the pretest probability of COVID-19 is high.

COVID-19肺炎に関する知見（例えば浸潤影やスリガラス状陰影）はこの病気に特異的なものではなく、むしろ、様々な感染性・非感染性の肺炎において広く見られるものである。従って、CT陽性という結果は、COVID-19の検査前確率が高い場合にのみ信憑性がある。

> The American College of Radiology helped to resolve this confusion with guidelines for the use of imaging for suspected COVID-19 infection in mid-March (last updated March 22) ([7: ACR Recommendations](https://www.acr.org/Advocacy-and-Economics/ACR-Position-Statements/Recommendations-for-Chest-Radiography-and-CT-for-Suspected-COVID19-Infection)). 

アメリカ放射線学会はこの混乱を解決するために、COVID-19感染が疑われる場合に対するイメージングの使用についてのガイドラインを3月中旬に発表した(下記)。

## ["ACR Recommendations for the use of Chest Radiography and Computed Tomography (CT) for Suspected COVID-19 Infection", 22 Mar](https://www.acr.org/Advocacy-and-Economics/ACR-Position-Statements/Recommendations-for-Chest-Radiography-and-CT-for-Suspected-COVID19-Infection)

>  The Centers for Disease Control (CDC) does not currently recommend CXR or CT to diagnose COVID-19. Viral testing remains the only specific method of diagnosis. Confirmation with the viral test is required, even if radiologic findings are suggestive of COVID-19 on CXR or CT.

> Generally, the findings on chest imaging in COVID-19 are not specific, and overlap with other infections, including influenza, H1N1, SARS and MERS.

> Based on these concerns, the ACR recommends:  
・CT should not be used to screen for or as a first-line test to diagnose COVID-19  
・    (Updated March 22, 2020) As an interim measure, until more widespread COVID-19 testing is available, some medical practices are requesting chest CT to inform decisions on whether to test a patient for COVID-19, admit a patient or provide other treatment. The ACR strongly urges caution in taking this approach. A normal chest CT does not mean a person does not have COVID-19 infection - and an abnormal CT is not specific for COVID-19 diagnosis. A normal CT should not dissuade a patient from being quarantined or provided other clinically indicated treatment when otherwise medically appropriate. Clearly, locally constrained resources may be a factor in such decision making.


## ["新型コロナウイルス肺炎(COVID-19)に対するCT検査については慎重な対応を", 井田, 19 Feb](https://jcr.or.jp/covid19_2020/covid-19_200218/) 


## ["Radiological findings from 81 patients with COVID-19 pneumonia in Wuhan, China: a descriptive study", Shi et al., Lancet, 24 Feb](https://www.sciencedirect.com/science/article/pii/S1473309920300864)

> COVID-19 pneumonia manifests with chest CT imaging abnormalities, even in asymptomatic patients, with rapid evolution from focal unilateral to diffuse bilateral ground-glass opacities that progressed to or co-existed with consolidations within 1–3 weeks. Combining assessment of imaging features with clinical and laboratory findings could facilitate early diagnosis of COVID-19 pneumonia.

## ["Longitudinal CT Findings in COVID-19 Pneumonia: Case Presenting Organizing Pneumonia Pattern", Wu et al., Radiology, 14 Feb](https://pubs.rsna.org/doi/10.1148/ryct.2020200031)


## ["Chest CT Findings in Coronavirus Disease-19 (COVID-19): Relationship to Duration of Infection", Bernheim, A. et al., Radiology, Feb 20](https://pubs.rsna.org/doi/full/10.1148/radiol.2020200463)

>  Indeed, given the limited number of rRT-PCR kits in some centers and the possibility of false negative rRT-PCR results, the National Health Commission of the People’s Republic of China has encouraged diagnosis based on clinical and chest CT findings alone [28]. 

実際、rRT-PCRキットの数が限られているため、rRT-PCRの結果が偽陰性になる可能性を考慮して、中国の国家衛生委員会は臨床初見と胸部CTのみに基づく診断を推奨している [National Health Commission of the People’s Republic of China website](www.en.nhc.gov/cn/2020-02/13/c_76515.htm)。

> In this study of 121 patients with confirmed Covid-19 infection, it is noteworthy that 20/36 (56%) of patients imaged 0-2 days (‘early’) after symptom onset had a normal CT with complete absence of ground-glass opacities and consolidation (as opposed to 3/33 [9%] of intermediate patients and 1/25 [4%] of late patients). 

この研究では、COVID-19の感染が確認された121名を対象に行われた。症状発症後すぐ(0-2日, "初期")に撮影されたCT画像において20/36 (56%) の患者においてすりガラス陰影も浸潤影も観察され**なかった**ことは注目に値する（これは中期の患者の3/33(9%)、後期の患者の1/25(4%)にも当てはまった）。

> Only one of these patients (who was in the early group) had an initially negative rRT-PCR result, suggesting that rRT-PCR is positive even in patients with normal chest CT. 

このうちrRT-PCRで陰性だったのは初期グループに含まれるたった1人だった。すなわち胸部CTが正常な患者であってもrRT-PCRでは陽性だった。

> Chest CT therefore has limited sensitivity and negative predictive value early after symptom onset, and is thereby unlikely a reliable standalone tool to rule out COVID-19 infection.

従って、症状発症初期では胸部CTの感度と陰性予測能力には限界があり、COVID19の感染を除外するための信頼に足る単独のツールとはいえない。


## ["Sensitivity of Chest CT for COVID-19: Comparison to RT-PCR", Fang et al., Radiology, 19 Feb](https://pubs.rsna.org/doi/10.1148/radiol.2020200432)

> In a series of 51 patients with chest CT and RT-PCR assay performed within 3 days, the sensitivity of CT for COVID-19 infection was 98% compared to RT-PCR sensitivity of 71% (p<.001).

## ["CT Imaging Features of 2019 Novel Coronavirus (2019-nCoV)", Chung et al., Radiology, 04 Feb](https://pubs.rsna.org/doi/full/10.1148/radiol.2020200230)

> Of 21 patients with the 2019 novel coronavirus, 15 (71%) had involvement of more than two lobes at chest CT, 12 (57%) had ground-glass opacities, seven (33%) had opacities with a rounded morphology, seven (33%) had a peripheral distribution of disease, six (29%) had consolidation with ground-glass opacities, and four (19%) had crazy-paving pattern.

> Fourteen percent of patients (three of 21) presented with a normal CT scan.

## ["Chest CT for Typical 2019-nCoV Pneumonia: Relationship to Negative RT-PCR Testing", Xie et al., Radiology, 12 Feb](https://pubs.rsna.org/doi/10.1148/radiol.2020200343)

> In patients at high risk for 2019-nCoV infection, chest CT evidence of viral pneumonia may precede positive negative RT-PCR test results.


## ["Rates of Co-infection Between SARS-CoV-2 and Other Respiratory Pathogens"](https://jamanetwork.com/journals/jama/fullarticle/2764787?guestAccessKey=6f011e04-8692-45d4-86d6-765d0528e063&utm_source=silverchair&utm_medium=email&utm_campaign=article_alert-jama&utm_content=olf&utm_term=041520)

## ["CRISPR–Cas12-based detection of SARS-CoV-2", Broughton et al., NatBiotech, 16 April](https://www.nature.com/articles/s41587-020-0513-4?utm_source=twitter&utm_medium=social&utm_content=organic&utm_campaign=NGMT_USG_JC01_GL_NRJournals)

> Visualization of the Cas12 detection reaction is achieved using a FAM-biotin reporter molecule and lateral flow strips designed to capture labeled nucleic acids (Fig. 2a)12. Uncleaved reporter molecules are captured at the first detection line (control line), whereas indiscriminate Cas12 cleavage activity generates a signal at the second detection line (test line). To compare the signal generated by Cas12 when using fluorescence or lateral flow, we carried out RT–LAMP using 5-fM or 0-fM IVT template using N gene primers and monitored the performance of the Cas12 readout on identical amplicons using a fluorescent plate reader and by lateral flow at 0, 2.5, 5 and 10 min (Fig. 2b,c). The Cas12 fluorescent signal was detectable in <1 min and a visual signal by lateral flow was achieved within 5 min.

> Fig.2e: Patient sample DETECTR data from lateral flow readout. Clinical samples from six patients with COVID-19 infection (n = 11, 5 replicates) and 12 patients infected with influenza or one of the four seasonal coronaviruses (HCoV-229E, HCoV-HKU1, HCoV-NL63, HCoV-OC43) (n = 12) were analyzed using SARS-CoV-2 DETECTR assay. Signal intensities from lateral flow strips were quantified using ImageJ and normalized to the highest value within the N gene, E gene or RNase P set, with a positive threshold set at 5 × s.d. above background. The final determination for SARS-CoV-2 test results was based on the interpretation matrix in Fig. 1e, with results indicated above the heat map. 

## ["Detection of 2019 novel coronavirus (2019-nCoV) by real-time RT-PCR", Corman et al., Eurosurveillance, 23 Jan](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.3.2000045)

## ["Detection of 2019 novel coronavirus (2019-nCoV) by real-time RT-PCR", Corman et al., Eurosurveillance, 23 Jan](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2020.25.3.2000045)

## ["Serial Interval of COVID-19 among Publicly Reported Confirmed Cases", Du et al., CDC EID, 19 Mar](https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article)

## ["Virological assessment of hospitalized patients with COVID-2019", Wolfel et al., Nature, 01 April](https://www.nature.com/articles/s41586-020-2196-x)

## ["Endothelial cell infection and endotheliitis in COVID-19", Varga et al., LANCET, 20 April](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30937-5/fulltext)

> SARS-CoV-2 infects the host using the angiotensin converting enzyme 2 (ACE2) receptor, which is expressed in several organs, including the lung, heart, kidney, and intestine. ACE2 receptors are also expressed by endothelial cells. Whether vascular derangements in COVID-19 are due to endothelial cell involvement by the virus is currently unknown. 

SARS-CoV-2のホストへの感染はアンジオテンシン変換酵素2 (ACE2) 受容体を介する。これは肺・心臓・腎臓・腸などを含むいくつかの臓器において発現が知られている。ACE2は、また、血管内皮細胞(endothelial cells)においても発現することが報告されている。COVID-10の血管障害 (vascular derangements) がウィルスによる血管内皮細部に関連しているかどうかは現時点では不明である。

> Here we demonstrate endothelial cell involvement across vascular beds of different organs in a series of patients with COVID-19

ここでは、COVID-10患者シリーズにおける複数の臓器にまたがる血管床の血管内皮細胞の関与について示した


## ["Is antibody-dependent enhancement playing a role in COVID-19 pathogenesis?" Francesco, Swiss Med Weekly, 16 April](https://smw.ch/article/doi/smw.2020.20249)
