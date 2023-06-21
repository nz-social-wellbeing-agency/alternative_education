/* 
SWA IDI condition indicator

Indicator: Autism Spectrum Disorder
Condition type: Effectively congential - present from birth / non-remitting
Refresh: [IDI_Clean_202210]
Quality: appears to reach prevalent capture for 10-18 year olds in 2021

The purpose of this code is to generate an indicator per person of whether someone has evidence of Autism Spectrum Disorder.
This evidence can be a:
1. diagnosis; and/or
2. treatment solely indicated for autism spectrum disorder; and/or
3. treatment indicated for ASD and other conditions (marker of higher risk).

This indicator relies only on category 1 data sources.

This was code has been lightly modified to be used to evaluate Alternative Education. 

--context: although ASD is not congenital, it's sysmptoms are often seen in the first year of life and as such can be treated as such
--more to the point, even if the date of diagnosis is recorded at later ages, like 9 years for example, one can treat it such that the impacts of the condition are opportating from 
--the first few years of life

--this code estimates lower prevalence before age 9 and greater than age 19 years

Author: C Wright
Created date: 20/10/2021

Change log
13/02/2023	By Andrew Webber
-I updated the IDI refresh to 202210, and changed the reference to the ultimate sandpit table to [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]
-The original code had extensive analysis seeking to validate the indicator (eg by comparing to other data sources). I have removed this, because that code did not modify the indicator.
-I have otherwise made no changes in the logic of the code
02/05/2023 By Craig Wright
-add some documentation and context 
-add code to estimate prevalence for validation purposes
-added PRIMHD provisional diagnoses (type='P')

*/
--FINAL ASD CONDITION TABLE: - one row per person with an asd diagosis
--this includes anyone, irrespective if alive or dea or migrated
--see code for linking to srp-ref table to calculate age specific prevalence

select * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]

--Original notes from Craig follow:

--ICD-10 F84.2 Rhett disease or RTT is not included as ASD in this code

--ASD definitions:

--NB PRIMHD team description has no value for ASD

--1: (any diagnosis of ASD) all data and sources
--2: (subset of diagnoses of ASD) data after2009 and only from sources: 

--ASD autism spectrum
--code_sys
--06 ICD-9-CM
--07 DSM IV
--10 ICD-10
--11 ICD-10
--12 ICD-10
--13 ICD-10
--14 ICD-10

--70 Health Specialty Code
--71 PRIMHD Team Type Code
--80 SOCRATES Diangosis - 1211,1206,1207
--81 IRAI diagnosis
--82 MSD Incap
--87 Gate way assessment

--1. Public hospital discharge
--2. Private hospital discharge
--3. MHINC ICD / DSM Codes - mha services till 1 july 2008
--4. PRIMHD - mha services after 1 july 2008
--5. MOH SOCRATES - MOH dss nasc data

--ACC, MOE, NNPAC, interrai and MHINC teams do not have sufficient detail to use AS SOURCES OF DIANOGSES

--MOH PHARMACEUTICALS DO NOT HAVE DRUGS SOLELY PRESCRIBED TO PEOPLE WITH ASD 
--THEY DO HAVE DRUGS FOR TREATENT OF SYMPTOMS THAT ARE COMMON FOR PEOPLE WITH ASD
--BUT THEY ARE PRESCRIBED FOR OTHER CONDITIONS AS WELL

--TYPE : this variable is used to indicate where the data source is:
--(not need for ASD but used for other conditions)
--S = solely indicated for the condition
--M = indicated for multiple conditions including the condition

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--NAP non-admitted patient collection : ED , outpatient an community services

--purchase unit lookup:
SELECT TOP (1000) [PURCHASE_UNIT_CODE]
      ,[PU_DESCRIPTION]
      ,[START_DATE]
      ,[END_DATE]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moh_nnpac_purchase_unit]
  where [PU_DESCRIPTION] like '%ASD%' or [PU_DESCRIPTION] like '%autism%'
  or [PURCHASE_UNIT_CODE]='DSS221'  or [PURCHASE_UNIT_CODE]='DSS220A'

  SELECT TOP (1000) [PURCHASE_UNIT_CODE]
      ,[PU_DESCRIPTION]
      ,[START_DATE]
      ,[END_DATE]
  FROM [IDI_Metadata_202210].[moh_nnpac].[purchase_unit19_code]
  where [PU_DESCRIPTION] like '%ASD%' or [PU_DESCRIPTION] like '%autism%'
  or [PURCHASE_UNIT_CODE]='DSS221'  or [PURCHASE_UNIT_CODE]='DSS220A'

--equivalent purchase unit codes: code and description of unit of service
--DSS221 assessment of ASD and further support
--DSS220A 

--MOH NPP non admitted patent collection 
--NO ASD RELATED PUS IN NAP TABLE

drop table if exists #npp_asd
SELECT [snz_uid]
      ,[moh_nnp_service_date]
      ,[moh_nnp_purchase_unit_code]
	  ,case when [moh_nnp_purchase_unit_code] in ('DSS220A') then 'S' else 'M' end as type
into #npp_asd
  FROM [IDI_Clean_202210].[moh_clean].[nnpac]
  where [moh_nnp_purchase_unit_code] in ('DSS220A','DSS221')

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

--PRIMHD diangosis codes DSM IV/ ICD 10
--SPECIALIST ACUTE MENTAL HEALTH AND ADICTION SERVICES

drop table if exists #moh_primhd_code

  SELECT b.snz_uid
	  	  ,'PRIMHD' as source
	  ,'ASD' as type
  
      --,[CLASSIFICATION_END_DATE]
      --,[CLINICAL_CODING_SYSTEM_ID]
      --,[CLINICAL_CODE_TYPE]
      --,[DIAGNOSIS_GROUPING_CODE]
	  --,[clinical_CODing_system_ID]
	  ,case 
	  when [clinical_CODing_system_ID]=10 then '10'
	  when [clinical_CODing_system_ID]=11 then '11'
	  when [clinical_CODing_system_ID]=12 then '12'
	  when [clinical_CODing_system_ID]=13 then '13'
	  when [clinical_CODing_system_ID]=14 then '14'
	  when [clinical_CODing_system_ID]=7 then '07'
	  when [clinical_CODing_system_ID]=6 then '06'
	  end as code_sys_1
	  ,[DIAGNOSIS_TYPE] as code_sys_2
      ,convert(date,[CLASSIFICATION_START_DATE],103)  as date
      ,[CLINICAL_CODE] code
      ,'Not available' as description
	  into #moh_primhd_code
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[primhd_diagnoses] as a,[IDI_Clean_202210].[moh_clean].[pop_cohort_demographics] as b
  where a.[snz_moh_uid]=b.[snz_moh_uid] and
      ((substring([CLINICAL_CODE],1,4) in ('F840','F841','F843','F845','F848','F849') and [clinical_CODing_system_ID]>='10' and [DIAGNOSIS_TYPE] in ('A','B','P')) or
  ((substring([CLINICAL_CODE],1,4) in ('2990','2991','2998')) and [clinical_CODing_system_ID]='06' and [DIAGNOSIS_TYPE] in ('A','B','P')) or
  ((substring([CLINICAL_CODE],1,5) in ('29900','29910','29980')) and [clinical_CODing_system_ID]='07' and [DIAGNOSIS_TYPE] in ('A','B','P')) )


--MOH SOCRATES 1211,1206,1207/ note issue with dates
--SOCRATES - MOH DSS NASC DATA - NEEDS ASSESMENT AND SERVICE COORDINATION

SELECT DISTINCT CODE,Description
FROM [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_disability] as a 
  where code in ('1211','1206','1207') 

--  CODE	Description
--1211	Autistic Spectrum Disorder (ASD)
--1206	Asperger's syndrome
--1207	Retired - Other autistic spectrum disorder (ASD)

drop table if exists #moh_soc_id
SELECT distinct b.snz_uid
	  ,'SOC' as source 
	  ,'ASD' as type
	  	  ,'80' as code_sys_1
		  ,'D' as code_sys_2
	  ,case when cast(substring([FirstContactDate],1,7) as date) is not null then cast(substring([FirstContactDate],1,7) as date) 
	  when cast(substring([ReferralDate],1,7) as date) is not null then cast(substring([ReferralDate],1,7) as date) 
	  end as date
	  ,cast(code as varchar(7)) as code
      ,[Description] 
	  into #moh_soc_id
  FROM [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_disability] as a 
  left join [IDI_Clean_202210].[moh_clean].[pop_cohort_demographics] as b on a.snz_moh_uid=b.snz_moh_uid 
  left join (select distinct snz_moh_uid,[FirstContactDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_needs_assessment] ) as c on a.snz_moh_uid=c.snz_moh_uid 
  left join (select distinct snz_moh_uid,[ReferralDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_referral]) as e on a.snz_moh_uid=e.snz_moh_uid
  where code in ('1211','1206','1207') and a.snz_moh_uid=c.snz_moh_uid 


--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

--PUBLIC HOSPITAL DISCHARGES
--ICD9 AND ICD10 ASD CODES

drop table if exists #moh_pub
SELECT 
	  [snz_uid]
	  ,'PUB' as source
	  ,'ASD' as type
--TOP (1000) [moh_dia_event_id_nbr]
--      ,[moh_dia_clinical_sys_code]
--      ,[moh_dia_submitted_system_code]
	  ,[moh_dia_submitted_system_code] as code_sys_1
      ,[moh_dia_diagnosis_type_code] as code_sys_2
	  ,[moh_evt_evst_date] as date
      ,[moh_dia_clinical_code] as code
	  ,'Not avaiable' as description
	  into #moh_pub
  FROM [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_diag] as a , [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_event] as b

  where [moh_dia_clinical_sys_code] = [moh_dia_submitted_system_code] and [moh_evt_event_id_nbr]=[moh_dia_event_id_nbr] and 
    ((substring([moh_dia_clinical_code],1,4) in ('F840','F841','F843','F845','F848','F849') and [moh_dia_clinical_sys_code]>='10' and [moh_dia_diagnosis_type_code] in ('A','B')) or
  ((substring([moh_dia_clinical_code],1,4) in ('2990','2991','2998')) and [moh_dia_clinical_sys_code]>='06' and [moh_dia_diagnosis_type_code] in ('A','B')) or
  ((substring([moh_dia_clinical_code],1,5) in ('29900','29910','29980')) and [moh_dia_clinical_sys_code]>='07' and [moh_dia_diagnosis_type_code] in ('A','B')) )

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  	--private hospital discharges / ICD9 / ICD10 ID codes

	drop table if exists #moh_pri_code
	SELECT a.[snz_uid]
	  	  	  ,'PRI' as source
	  ,'ASD' as type
	  	  ,[moh_pri_diag_sub_sys_code] as code_sys_1
	  	  ,[moh_pri_diag_diag_type_code] as code_sys_2
      ,cast([moh_pri_evt_start_date] as date) as date
      ,[moh_pri_diag_clinic_code] as code
	  	  ,'Not avaiable' as description
      --,[moh_pri_diag_op_ac_date]
  into #moh_pri_code
  FROM [IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_event] as a,[IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_diag] as b
  where a.[moh_pri_evt_event_id_nbr]=b.[moh_pri_diag_event_id_nbr] and [moh_pri_diag_clinic_sys_code]=[moh_pri_diag_sub_sys_code] and 
  ((substring([moh_pri_diag_clinic_code],1,4) in ('F840','F841','F843','F845','F848','F849') and [moh_pri_diag_sub_sys_code]>='10' and [moh_pri_diag_diag_type_code] in ('A','B')) or
  ((substring([moh_pri_diag_clinic_code],1,4) in ('2990','2991','2998')) and [moh_pri_diag_sub_sys_code]='06' and [moh_pri_diag_diag_type_code] in ('A','B')))

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MHINC and PRIMHD diagnoses

  drop table if exists #moh_mhinc_code
  SELECT b.snz_uid
  ,'MHINC' as source
	  ,'ASD' as type
	  	  ,[clinical_coding_system_id] as code_sys_1
	  	  ,cast(diagnosis_type as varchar(2)) as code_sys_2
      ,[classification_start] as date
      ,[CLINICAL_CODE] as code
	  ,'Not avaiable' as description
	  into #moh_mhinc_code
	  --ADHOC TABLE HAS NHI / SNZ_MOH_UID SO NEED TO JOIN TO SECURITY CONDCORDANCE OF CURRENT REFRESH TO ADD SNZ_UID
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[moh_primhd_mhinc] as a, [IDI_Clean_202210].[security].[concordance] as b
  where
  ((substring([CLINICAL_CODE],1,4) in ('F840','F841','F843','F845','F848','F849') and [clinical_coding_system_id]>='10') or
  ((substring([CLINICAL_CODE],1,4) in ('2990','2991','2998')) and [clinical_coding_system_id]>='06') or
  ((substring([CLINICAL_CODE],1,5) in ('29900','29910','29980')) and [clinical_coding_system_id]>='07') 
  )
  and a.snz_moh_uid=b.snz_moh_uid

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--cyf gateway assessment resulting in a diagnosis of ASD or autism

  drop table #gw_asd
SELECT 
b.snz_uid,
[snz_prsn_uid]
,'ASD' as type
,'CYF_GW' as source
      ,[snz_current_prsn_uid]
      ,a.[snz_msd_uid]
      ,[snz_gateway_uid]
      ,[need_selection_type_code]
      ,[need_type_code] as code
	  ,'87' as code_sys_1
	  ,'GW' as code_sys_2
      ,[needs_desc] as description
      ,[need_category_code]
      ,[needs_cat_desc]
      ,[education_yn]
      ,[health_yn]
      ,[needs_created_date] date
      ,[extract_date]
  into #gw_asd
	  --ADHOC TABLE HAS NHI / SNZ_MOH_UID SO NEED TO JOIN TO SECURITY CONDCORDANCE OF CURRENT REFRESH TO ADD SNZ_UID

  FROM [IDI_Adhoc].[clean_read_CYF].[cyf_gateway_cli_needs] as a ,IDI_Clean_202210.security.concordance as b
  where a.snz_msd_uid=b.snz_msd_uid and ([needs_desc] like '%autism%'  )

--  combine all diagnoses for ASD into one record person snz_uid

drop table if exists #asd

select snz_uid,min(date) as min_date
into #asd
from (
select snz_uid,source,type,code,date,description,code_sys_1,code_sys_2 from #moh_mhinc_code
UNION ALL
select snz_uid,source,type,code,date,description,code_sys_1,code_sys_2 from #moh_PRIMHD_code
UNION ALL
select snz_uid,source,type,code,date,description,code_sys_1,code_sys_2 from #moh_soc_id
UNION ALL
select snz_uid,source,type,code,date,description,code_sys_1,code_sys_2 from #moh_pub
UNION ALL
select snz_uid,source,type,code,date,description,code_sys_1,code_sys_2 from #moh_pri_code
) as a
group by snz_uid



--output the master table and add demographics in case it is usefull

drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]

select a.*,b.snz_birth_year_nbr,b.snz_deceased_year_nbr,b.snz_birth_date_proxy,b.snz_sex_gender_code
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]
from #asd as a, idi_clean_202210.data.personal_detail as b
where a.snz_uid=b.snz_uid


--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

--estimate asd prevalence by age for 2021 population
--note that diagnoses occur later in life , not at birth so prevalence for recent years is lower for younger age groups
select age,count(*) as ppl,sum(asd) as asd,sum(asd*100.00)/count(*) as prevalence
from (
select a.*,case when b.snz_uid is not null then 1 else 0 end as asd,
floor(datediff(day,c.snz_birth_date_proxy,srp_ref_date)/365.24) as age
from IDI_Clean_202210.data.snz_res_pop as a left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] as b
on a.snz_uid=b.snz_uid
left join IDI_Clean_202210.data.personal_detail as c on a.snz_uid=c.snz_uid
where year(srp_ref_date)=2021
) as a
group by age
order by age


select count(*) as ppl,sum(asd) as asd,sum(asd*100.00)/count(*) as prevalence
from (
select a.*,case when b.snz_uid is not null then 1 else 0 end as asd,
floor(datediff(day,c.snz_birth_date_proxy,srp_ref_date)/365.24) as age
from IDI_Clean_202210.data.snz_res_pop as a left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] as b
on a.snz_uid=b.snz_uid
left join IDI_Clean_202210.data.personal_detail as c on a.snz_uid=c.snz_uid
where year(srp_ref_date)=2021
) as a


--estimate asd prevalence by age for 2021 population
--note that running prevalence on an earlier year and assuming asd is effectively congenital one starts to see sensible prevalence figures for younger ages
select age,count(*) as ppl,sum(asd) as asd,sum(asd*100.00)/count(*) as prevalence
from (
select a.*,case when b.snz_uid is not null then 1 else 0 end as asd,
floor(datediff(day,c.snz_birth_date_proxy,srp_ref_date)/365.24) as age
from IDI_Clean_202210.data.snz_res_pop as a left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] as b
on a.snz_uid=b.snz_uid
left join IDI_Clean_202210.data.personal_detail as c on a.snz_uid=c.snz_uid
where year(srp_ref_date)=2016
) as a
group by age
order by age

