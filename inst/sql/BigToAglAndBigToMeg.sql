IF OBJECT_ID('@results_database_schema.studyCohort', 'U') IS NOT NULL DROP TABLE @results_database_schema.studyCohort;
CREATE TABLE @results_database_schema.studyCohort (cohort_definition_id INT,  subject_id BIGINT, cohort_start_date DATE, cohort_end_date DATE);

CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (19107329,40163921,40163922,40163923,40163924,40163925,40163926,40163927,40163928,40163929,1503297,1503327,19086484,19106521,19088818,1503328,19109284,40063350,40117785,40153151,40135481,40164884,40164894,40164895,40164896,40164899,40164900,40164901,40164897,40164898,40164880,40164881,40164882,40164883,40164902,40164903,40164925,40164926,40164927,40164928,40164931,40167189,40164932,40164933,40167190,40164929,19006932,40164930,19027257,40164934,40164937,40164935,40164936,40164938,40164939,40164940,40164941,40164942,40176046,40167191,40164948,40167192,40167193,40164946,19006933,40164947,19121940,19027259,40063352,40063353,40063354,40063355,40063356,40126695,40063357)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (1529331,1529358,19021312,19040970,19021787,1529359,19023063,19047612,19120257,1529360,1529352,19040969,19021786,40003412,40003413,1510202,19081704,19067232,19029061,1510208,19081705,19067200,19029029,1510206,19081706,19067231,19029030,1510207,40056705,40056706)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (46221553,46221557,1596974,1596975,1596972,1596976,1596973,19135320,19129443,19129469,46275390,46275452,45777024,45777025,46234041,46234043,46234047,46234049,1567234,40176981,1516980,1516981,19135276,1502910,1544872,1544873,46234237,1596960,1596959,19135263,1596956,1596957,46233974,46233976,46233969,46233970,46233945,46233948,1513915,1513916,1513913,1513914,19135264,19135265,45777022,45777023,19129439,19129442,19122121,1567198,1567200,46234044,46234045,19078558,1567232,19005729,46234050,46234051,19099834,19133792,46234040,46234042,40051314,40051316,46234046,46234048,1531601,19133791,19133793,1531623,1567201,40184293,40184294,42902356,42903270,40051007,40184291,46234052,46234053,19095212,19117090,19078606,19017221,19014975,19014972,19014976,19061587,19095255,19120825,19069663,19078604,19078529,1515107,19095213,19078605,1516976,19112694,1516978,1516979,19044971,42902821,42903168,40056632,40130712,46234190,46234191,1502905,1502908,19078559,19071700,19120790,42902742,42902743,46221552,46221558,46221559,46221555,40051347,40051348,46234097,46234098,46234099,1544838,19112790,19131581,19112791,19131582,42902468,42902371,40051353,40153110,46233875,46233876,19095256,19116794,19116795,19062456,19078552,19012702,19012703,19078530,19078551,19012270,19063033,46234239,46234240,42902823,42902606,42902571,19116730,19110219,19104173,19110352,19104457,19110354,19104458,19111795,19104469,19110355,19104459,19111797,19104470,1596941,1596932,1596940,19117028,19116559,1596918,19010533,19010204,19009388,19116451,1596955,19116613,19116455,1596921,19010534,19009422,19095214,19116611,19120315,19078603,19010532,19009384,1596916,19062999,46234234,46234235,42903059,42902607,42903113,19100127,19101876,19009387,19116450,1596954,19116610,19116454,1596920,19010531,19009421,1596953,1596917,19009386,19116449,1596952,19116609,19116453,1596919,19010510,19009390,1588986,19129428,19129431,19129437,19129440,19129433,42903044,19129435,19129438,19129441,40148823,40148825,40148826,46221581,40051363,40051374,40052043,40052044,40052045,40051364,40052072,40051375,40052047,40052073,40052075,40052048,40052050,40052051,40050609,40052053,46234236,46234238,40159543,40159544,40159545,40051377,40051379,40051382,40051689,40051690,46234216,46234233,40159519,40159520,40159521,1550023,19095211,46233977,46233978,19016117,19058398,40166274,19123376,19123375,46233971,46233972,46233943,46233946,46233949,46233950,1550027,19018822,1513877,1513912,42902587,42902923,1550028,19033127,19067324,1513881,42902945,42902599,46233973,46233975,40052088,40166275,40130843,40052077,40143226,46234016,46234017,46233944,46233947,42899447,19095215,19117088,19078607,19017199,19078553,19016044,19062719,19116884,19095472,1586327,19100131,19116452,19115111,19009389,19095216,19117089,1586345,19017200,19016043,19116883,1586344,1586328,19090244,19090246,19090245,40052093,19090229,19090242,19078634,19090243,19078635,40052099,19090247,19110359,19104432,19090248,1586371,40052361,40051333,19090249,19095477,19078636,19110361,19104442,40052370,1513876,1513879,1513880,19090180,19094096,19116446,19090181,19009383,19094097,19061541,19094098,19061627,40052382,40052383,19013926,19094042,19078608,19094043,19080064,40052389,19091621,19091623,19091622,40174828,40174829,40174830,40174831,40052393,40174832,19090187,19094099,19090188,19094100,19061545,19094143,19061546,40052401,19013951,19095217,19116444,19078631,19009381,19013956,19061543,19013957,19061624,40052410,40052411,1590165,19094016,19069665,19078609,1590170,19094020,19078610,19110358,19104451,40052422,40052423,40052425,1586346,19094017,19117087,19069640,19078632,19017198,1586367,19016047,19116886,19110376,19104091,19095219,19094124,19078633,19094045,19069661,19028937,19098280,19100132,19094125,19061544,40052794,40052795,40052796,40052798,1513849,19095254,19116792,19116729,19062454,1513850,19012680,19012269,19078526,19078527,19063031,19110360,19095473,1513851,40053113,40053114,40053115,40053116,40053117,1562586,19095200,19069638,19082658,19078528,19012268,19116728,1562623,19061547,43525653,19072747,19095474,19061548,40053128,40053129,40053131,43525383,19090204,19090208,19116445,19078601,19009382,19090209,19090206,19090210,19090205,40052808,40052809,1513843,1513848,19062511,19021048,1513845,19012267,19112802,19116727,19112801,19095199,19078560,40052820,40052821,40052822,40052823,1596977,19094121,19116790,19116760,19062458,19116791,19078555,19012678,19012507,19078557,19012679,19078554,19012266,19078556,19063035,42903369,42903322,19116726,19007871,1597001,46234241,46234243,46234242,46234244,19110353,1597000,1597012,19110220,19100128,19095218,19094122,19061542,46233728,46233729,45776621,45776625,19111798,19094123,19062459,19061670,1596999,1596998,19110357,19104092,19110356,46233730,46233731,45774398,45774400,19111796,45776620,45776624,40052768,40052770,40052769,40052771,40052772,40052773,40052774,40052775,40159688,40159689,19090226,19095476,19078639,40051323,19090221,19099012,19078637,19099013,19078638,40051331,1586369,1586383,19120831,19116793,1586370,19080091,19012701,19095475,40051340,40051341,40051342)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4140466,43531588,45769888,45763582,43531578,43531559,43531566,43531653,43531577,43531562,45769894,4313070,4221495,43531564,4312009,43531563,443733,43530689,4226121,43530653,43531608,43531597,443732,45757280,45769906,4177050,4223463,43530690,45769890,4222876,45772019,45769889,45770880,4215719,45757392,45771064,45757447,45757446,45757445,45757444,45757363,45772060,4227824,45769875,4130162,45771072,443734,4228443,45770830,45769905,45757435,43531651,45770881,4222415,45769828,376065,45757450,45770883,45757255,43530656,45769836,443729,45757278,4221487,4223739,4226791,4063043,43531010,4129519,43530685,45770831,45757499,443731,45770928,45757075,45769872,45769835,4142579,45770832,45773064,201826,4230254,4304377,4321756,4196141,4099217,201530,4151282,4099216,4198296,4193704,4200875,4099651,45757277,45757449)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4143857,45769891,45763585,45773688,45773576,45769901,45771075,45769902,45769903,45769837,4137214,4225656,45769832,43531565,4312138,373999,43530657,4227210,45757074,435216,45769904,4221344,45757507,45769892,4223303,45771068,45757432,443592,45757393,45771067,45769876,4228112,45757362,439770,4224254,45757535,43530660,4225055,4224709,45769829,377821,45769830,45763583,45769834,318712,4222687,4222553,4224723,4063042,43531008,43531009,45763584,45757604,200687,45757266,45757073,45771533,45773567,45769833,4143689,45769873,201254,4099215,4152858,4096668,201531,4151281,443412,4295011,4099214,45770902)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
) C

WHERE C.ordinal = 1

  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,30,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

JOIN (
select 0 as index_id, event_id
FROM
(
  select event_id FROM
  (
    SELECT 0 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 1)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,0,P.START_DATE) and A.START_DATE <= DATEADD(day,90,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1


UNION
SELECT 1 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 2)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) = 0


UNION
SELECT 2 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 3)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1


UNION
SELECT 3 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 4)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) = 0


  ) CQ
  GROUP BY event_id
  HAVING COUNT(index_id) = 4
) G
) AC on AC.event_id = pe.event_id

) QE
WHERE QE.ordinal = 1
;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_one;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_one as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;




TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;


CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (19107329,40163921,40163922,40163923,40163924,40163925,40163926,40163927,40163928,40163929,1503297,1503327,19086484,19106521,19088818,1503328,19109284,40063350,40117785,40153151,40135481,40164884,40164894,40164895,40164896,40164899,40164900,40164901,40164897,40164898,40164880,40164881,40164882,40164883,40164902,40164903,40164925,40164926,40164927,40164928,40164931,40167189,40164932,40164933,40167190,40164929,19006932,40164930,19027257,40164934,40164937,40164935,40164936,40164938,40164939,40164940,40164941,40164942,40176046,40167191,40164948,40167192,40167193,40164946,19006933,40164947,19121940,19027259,40063352,40063353,40063354,40063355,40063356,40126695,40063357)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (1502826,19085096,1502829,19071495,19045606,19102037,1502873,19110957,19107111,19085097,1502830,1502827,19045605,19110958,19107110,40058198,40058199,1516766,1516795,19120528,19023425,19054443,1516770,19078028,19081914,19120529,19023424,19054444,1516771,19078029,1516794,19120530,19023426,19054445,19047663,19078030,40098705,40098706,40098707)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (46221553,46221557,1596974,1596975,1596972,1596976,1596973,19135320,19129443,19129469,46275390,46275452,45777024,45777025,46234041,46234043,46234047,46234049,1567234,40176981,1516980,1516981,19135276,1502910,1544872,1544873,46234237,1596960,1596959,19135263,1596956,1596957,46233974,46233976,46233969,46233970,46233945,46233948,1513915,1513916,1513913,1513914,19135264,19135265,45777022,45777023,19129439,19129442,19122121,1567198,1567200,46234044,46234045,19078558,1567232,19005729,46234050,46234051,19099834,19133792,46234040,46234042,40051314,40051316,46234046,46234048,1531601,19133791,19133793,1531623,1567201,40184293,40184294,42902356,42903270,40051007,40184291,46234052,46234053,19095212,19117090,19078606,19017221,19014975,19014972,19014976,19061587,19095255,19120825,19069663,19078604,19078529,1515107,19095213,19078605,1516976,19112694,1516978,1516979,19044971,42902821,42903168,40056632,40130712,46234190,46234191,1502905,1502908,19078559,19071700,19120790,42902742,42902743,46221552,46221558,46221559,46221555,40051347,40051348,46234097,46234098,46234099,1544838,19112790,19131581,19112791,19131582,42902468,42902371,40051353,40153110,46233875,46233876,19095256,19116794,19116795,19062456,19078552,19012702,19012703,19078530,19078551,19012270,19063033,46234239,46234240,42902823,42902606,42902571,19116730,19110219,19104173,19110352,19104457,19110354,19104458,19111795,19104469,19110355,19104459,19111797,19104470,1596941,1596932,1596940,19117028,19116559,1596918,19010533,19010204,19009388,19116451,1596955,19116613,19116455,1596921,19010534,19009422,19095214,19116611,19120315,19078603,19010532,19009384,1596916,19062999,46234234,46234235,42903059,42902607,42903113,19100127,19101876,19009387,19116450,1596954,19116610,19116454,1596920,19010531,19009421,1596953,1596917,19009386,19116449,1596952,19116609,19116453,1596919,19010510,19009390,1588986,19129428,19129431,19129437,19129440,19129433,42903044,19129435,19129438,19129441,40148823,40148825,40148826,46221581,40051363,40051374,40052043,40052044,40052045,40051364,40052072,40051375,40052047,40052073,40052075,40052048,40052050,40052051,40050609,40052053,46234236,46234238,40159543,40159544,40159545,40051377,40051379,40051382,40051689,40051690,46234216,46234233,40159519,40159520,40159521,1550023,19095211,46233977,46233978,19016117,19058398,40166274,19123376,19123375,46233971,46233972,46233943,46233946,46233949,46233950,1550027,19018822,1513877,1513912,42902587,42902923,1550028,19033127,19067324,1513881,42902945,42902599,46233973,46233975,40052088,40166275,40130843,40052077,40143226,46234016,46234017,46233944,46233947,42899447,19095215,19117088,19078607,19017199,19078553,19016044,19062719,19116884,19095472,1586327,19100131,19116452,19115111,19009389,19095216,19117089,1586345,19017200,19016043,19116883,1586344,1586328,19090244,19090246,19090245,40052093,19090229,19090242,19078634,19090243,19078635,40052099,19090247,19110359,19104432,19090248,1586371,40052361,40051333,19090249,19095477,19078636,19110361,19104442,40052370,1513876,1513879,1513880,19090180,19094096,19116446,19090181,19009383,19094097,19061541,19094098,19061627,40052382,40052383,19013926,19094042,19078608,19094043,19080064,40052389,19091621,19091623,19091622,40174828,40174829,40174830,40174831,40052393,40174832,19090187,19094099,19090188,19094100,19061545,19094143,19061546,40052401,19013951,19095217,19116444,19078631,19009381,19013956,19061543,19013957,19061624,40052410,40052411,1590165,19094016,19069665,19078609,1590170,19094020,19078610,19110358,19104451,40052422,40052423,40052425,1586346,19094017,19117087,19069640,19078632,19017198,1586367,19016047,19116886,19110376,19104091,19095219,19094124,19078633,19094045,19069661,19028937,19098280,19100132,19094125,19061544,40052794,40052795,40052796,40052798,1513849,19095254,19116792,19116729,19062454,1513850,19012680,19012269,19078526,19078527,19063031,19110360,19095473,1513851,40053113,40053114,40053115,40053116,40053117,1562586,19095200,19069638,19082658,19078528,19012268,19116728,1562623,19061547,43525653,19072747,19095474,19061548,40053128,40053129,40053131,43525383,19090204,19090208,19116445,19078601,19009382,19090209,19090206,19090210,19090205,40052808,40052809,1513843,1513848,19062511,19021048,1513845,19012267,19112802,19116727,19112801,19095199,19078560,40052820,40052821,40052822,40052823,1596977,19094121,19116790,19116760,19062458,19116791,19078555,19012678,19012507,19078557,19012679,19078554,19012266,19078556,19063035,42903369,42903322,19116726,19007871,1597001,46234241,46234243,46234242,46234244,19110353,1597000,1597012,19110220,19100128,19095218,19094122,19061542,46233728,46233729,45776621,45776625,19111798,19094123,19062459,19061670,1596999,1596998,19110357,19104092,19110356,46233730,46233731,45774398,45774400,19111796,45776620,45776624,40052768,40052770,40052769,40052771,40052772,40052773,40052774,40052775,40159688,40159689,19090226,19095476,19078639,40051323,19090221,19099012,19078637,19099013,19078638,40051331,1586369,1586383,19120831,19116793,1586370,19080091,19012701,19095475,40051340,40051341,40051342)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 3 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4140466,43531588,45769888,45763582,43531578,43531559,43531566,43531653,43531577,43531562,45769894,4313070,4221495,43531564,4312009,43531563,443733,43530689,4226121,43530653,43531608,43531597,443732,45757280,45769906,4177050,4223463,43530690,45769890,4222876,45772019,45769889,45770880,4215719,45757392,45771064,45757447,45757446,45757445,45757444,45757363,45772060,4227824,45769875,4130162,45771072,443734,4228443,45770830,45769905,45757435,43531651,45770881,4222415,45769828,376065,45757450,45770883,45757255,43530656,45769836,443729,45757278,4221487,4223739,4226791,4063043,43531010,4129519,43530685,45770831,45757499,443731,45770928,45757075,45769872,45769835,4142579,45770832,45773064,201826,4230254,4304377,4321756,4196141,4099217,201530,4151282,4099216,4198296,4193704,4200875,4099651,45757277,45757449)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 4 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4143857,45769891,45763585,4128019,45773688,45773576,45769901,45771075,45769902,45769903,45769837,4137214,4225656,45769832,43531565,4312138,373999,43530657,4227210,45757074,435216,45769904,4221344,45757507,45769892,4223303,45771068,45757432,443592,45757393,45771067,45769876,4228112,45757362,439770,4224254,45757535,43530660,4225055,4224709,45769829,377821,45769830,45763583,45769834,318712,4222687,4222553,4224723,4063042,43531008,43531009,45763584,45757604,200687,45757266,45757073,45771533,45773567,45769833,4143689,45769873,201254,4099215,198995,40484648,40484649,4152858,40584359,4096668,201531,4151281,40386757,4200873,40386756,443412,4295011,40575609,40518516,4099214,45770902)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 5 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4143857,45769891,45763585,45773688,45773576,45769901,45771075,45769902,45769903,45769837,4137214,4225656,45769832,43531565,4312138,373999,43530657,4227210,45757074,435216,45769904,4221344,45757507,45769892,4223303,45771068,45757432,443592,45757393,45771067,45769876,4228112,45757362,439770,4224254,45757535,43530660,4225055,4224709,45769829,377821,45769830,45763583,45769834,318712,4222687,4222553,4224723,4063042,43531008,43531009,45763584,45757604,200687,45757266,45757073,45771533,45773567,45769833,4143689,45769873,201254,4099215,4152858,4096668,201531,4151281,443412,4295011,4099214,45770902)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
) C

WHERE C.ordinal = 1

  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,30,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

JOIN (
select 0 as index_id, event_id
FROM
(
  select event_id FROM
  (
    SELECT 0 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 1)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= DATEADD(day,0,P.START_DATE) and A.START_DATE <= DATEADD(day,90,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1


UNION
SELECT 1 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.drug_exposure_start_date as start_date, COALESCE(C.drug_exposure_end_date, DATEADD(day, 1, C.drug_exposure_start_date)) as end_date, C.drug_concept_id as TARGET_CONCEPT_ID
from
(
  select de.*, ROW_NUMBER() over (PARTITION BY de.person_id ORDER BY de.drug_exposure_start_date) as ordinal
  FROM @cdm_database_schema.DRUG_EXPOSURE de
where de.drug_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 2)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) = 0


UNION
SELECT 2 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 3)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) >= 1


UNION
SELECT 3 as index_id, p.event_id
FROM #primary_events P
LEFT JOIN
(
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 5)
) C



) A on A.person_id = P.person_id and A.START_DATE >= P.OP_START_DATE AND A.START_DATE <= P.OP_END_DATE AND A.START_DATE >= P.OP_START_DATE and A.START_DATE <= DATEADD(day,0,P.START_DATE)
GROUP BY p.event_id
HAVING COUNT(A.TARGET_CONCEPT_ID) = 0


  ) CQ
  GROUP BY event_id
  HAVING COUNT(index_id) = 4
) G
) AC on AC.event_id = pe.event_id

) QE
WHERE QE.ordinal = 1
;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_two;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_two as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;




TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;


CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (3004410,3007263,3003309,3005673,40762352,40758583,3034639)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.measurement_date as start_date, DATEADD(d,1,C.measurement_date) as END_DATE, C.measurement_concept_id as TARGET_CONCEPT_ID
from
(
  select m.*, ROW_NUMBER() over (PARTITION BY m.person_id ORDER BY m.measurement_date) as ordinal
  FROM @cdm_database_schema.MEASUREMENT m
where m.measurement_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
) C

WHERE C.value_as_number <= 7.0000

  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

) QE

;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_three;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_three as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;
TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;

CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (45766075,4178129,4267568,44784623,312327,44782769,44782712,45766115,434376,45766150,438438,4243372,4108669,4151046,4275436,438170,45771322,438447,441579,436706,4324413,4051874,4303359,4147223,4270024,319039,4126801,4296653,46270162,46270163,43020460,43020461,45766076,46270159,46270160,45766116,45766151,46274044,46270161,46273495,46270158,46270164,4119947,4329847,4170094,4154704,4179525,4200113,4119949,4121467,4119950,314666,4121468,4322145,319038,4030582,4206867,4207921,4209541,4108679,4108219,4108220,4124686,4108217,4108677,4108218,45766241,45766114,45766113,45773170,4108680,439693)and invalid_reason is null

) I
) C;
INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (45766075,4178129,4267568,44784623,312327,44782769,44782712,45766115,434376,45766150,438438,4243372,4108669,4151046,4275436,438170,45771322,438447,441579,436706,4324413,4051874,4303359,4147223,4270024,319039,4126801,4296653,46270162,46270163,43020460,43020461,45766076,46270159,46270160,45766116,45766151,46274044,46270161,46273495,46270158,46270164,4119947,4329847,4170094,4154704,4179525,4200113,4119949,4121467,4119950,314666,4121468,4322145,319038,4030582,4206867,4207921,4209541,4108679,4108219,4108220,4124686,4108217,4108677,4108218,45766241,45766114,45766113,45773170,4108680,439693)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 1)
) C



  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

) QE

;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_four;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_four as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;
TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;

CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4323988,4109637,4109809,4109022,43531578,443614,43531559,443601,43531566,443597,45763854,45763855,43531653,443612,43531577,443611,43531562,46271022,46284566,46284567,46284570,46284572,46287169,46284575,46284587,46286992,46284588,46284591,46284592,46284593,46284597,46284598,46284599,46284600,46284602,46284603,198124,40480635,40483823,436232,201675,45771064,45757447,45757446,45757445,45757444)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
) C



  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

) QE

;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_five;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_five as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;
TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;

CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
(
  select concept_id from @cdm_database_schema.CONCEPT where concept_id in (4196110,4195495,4210131,4006784,4338899,4199963,372315,375256,4318983,4105159,4007451,4208203,4317977,4001501,4070182,4070183,375545,4301387,381566,376973,372894,4338904,4195496,4208201,4199038,4219330,376400,375546,4008296,378534,4103579,44783428,432895,4221495,4174977,4226121,4109548,4334886,4164176,379811,45757567,40487893,4210432,44812346,4195051,4210871,376399,40482507,45770919,4220818,373770,4319589,4161420,4109424,45770830,377285,378743,45757435,4152554,377552,45770881,4130588,4199942,377274,373769,4230391,439297,4235260,4255400,4252356,4215961,4252215,4246964,4255281,4247107,4255399,4255401,4212441,4218499,4048060,4335999,4197734,436976,438749,4323127,4105172,40479994,380096,43530685,45757065,4195043,4266042,4164174,4195044,4210128,4210129,4336000,4338900,45757798,4109401,45763584,45770831,4335998,4230930,4336003,4208211,376103,376114,4290822,4266637,4088107,4007944,4269871,4290823,4221962,4164632,4266041,46272745)and invalid_reason is null

) I
) C;

select row_number() over (order by P.person_id, P.start_date) as event_id, P.person_id, P.start_date, P.end_date, OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date
INTO #primary_events
FROM
(
  select P.person_id, P.start_date, P.end_date, ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY start_date ASC) ordinal
  FROM
  (
  select C.person_id, C.condition_start_date as start_date, COALESCE(C.condition_end_date, DATEADD(day,1,C.condition_start_date)) as end_date, C.CONDITION_CONCEPT_ID as TARGET_CONCEPT_ID
from
(
        select co.*, ROW_NUMBER() over (PARTITION BY co.person_id ORDER BY co.condition_start_date) as ordinal
        FROM @cdm_database_schema.CONDITION_OCCURRENCE co
where co.condition_concept_id in (SELECT concept_id from  #Codesets where codeset_id = 0)
) C



  ) P
) P
JOIN @cdm_database_schema.observation_period OP on P.person_id = OP.person_id and P.start_date between OP.observation_period_start_date and op.observation_period_end_date
WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= P.START_DATE AND DATEADD(day,0,P.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE AND P.ordinal = 1
;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date
INTO #qualified_events
FROM
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal
  FROM #primary_events pe

) QE

;


create table #inclusionRuleCohorts
(
  inclusion_rule_id bigint,
  event_id bigint
)
;


with cteIncludedEvents(event_id, person_id, start_date, end_date, op_start_date, op_end_date, ordinal) as
(
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusionRuleCohorts I on I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

)
select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM cteIncludedEvents Results
WHERE Results.ordinal = 1
;

-- Apply end date stratagies
-- by default, all events extend to the op_end_date.
select event_id, person_id, op_end_date as end_date
into #cohort_ends
from #included_events;

-- Date Offset Strategy
INSERT INTO #cohort_ends (event_id,  person_id, end_date)
select event_id, person_id,
  case when DATEADD(day,1000,start_date) > start_date then DATEADD(day,1000,start_date) else start_date end as end_date
from #included_events
;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id_six;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id_six as cohort_definition_id, F.person_id, F.start_date, F.end_date
FROM (
  select Q.person_id, Q.start_date, E.end_date, row_number() over (partition by Q.event_id order by E.end_date) as ordinal
  from #qualified_events Q
  join #cohort_ends E on Q.event_id = E.event_id and Q.person_id = E.person_id and E.end_date >= Q.start_date
) F
WHERE F.ordinal = 1
;
TRUNCATE TABLE #cohort_ends;
DROP TABLE #cohort_ends;

TRUNCATE TABLE #inclusionRuleCohorts;
DROP TABLE #inclusionRuleCohorts;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #primary_events;
DROP TABLE #primary_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;

