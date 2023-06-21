/****** Script for SelectTopNRows command from SSMS  ******/
--PURPOSE: ADHD incidence and prevalence
--aim is to have identify people with ADHD
--Author: C Wright
--date: 6 / 10 /2022

--NB: need to review drugs and indications

--Notes:
--version
--1.0 base version
--1.1 add gateway data
--1.2 create categories of evidence - diagnosis , drug treatment with other evidence, only drug treatment with stimulants used to treat ADHD
--screening with b4sc where hyperactivity scores for parent and techer are either 7+


--Diagnosis - Explicit diagnosis for ADHD
--Treatment OE - treated by a drug for ADHD that has other indications and some other ADHD type information , like hyperactivty
--Treatment NoOE - treated by a stimulant drug for ADHD that has other indications 
-- the ADHD drugs with additional indications: are for narcolepsy - rare in children 
--depression but rarely used in children over other SSRIss, chronic fatigue and congitive enhanement

--Screened A - has b4sc hperactivity score for parent or teacher of 7+
--Other - those who have a B4SC SDQ hyperactitiy scores of less than 7 

--neurodivergent



--classes of information:

--1. solely indicated for adhd
--2. used to treat adhd but also other things - can't use fr ID but can use for date of diangosis

--TO DO: 

--STEP 1: first date from sources of data that constitute a diagnosis
--Y hospital diagnosis code
--X MSD incapacitation code
--X Outpatient purchase units
--interrai diabetes indicator
--Y MHA MHINC/PRIMHD diagnosis code
--X SOCRATES diagnosis 


--STEP 2: get dates from sources that constitute treatment for MDD 
--X lab tests for ....
--X metformin dispensing - ....
--X hospital health specialty of .......
--X drug dispensing for drugs for MDD
--Y drug dispensing for MDD only



--ACC
--E2E0.	CHILD ATTENTION DEFICIT DISORDER
--E2E00	ATTENTION DEFICIT WITHOUT HYPERACTIVITY
--E2E01	ATTENTION DEFICIT WITH HYPERACTIVITY
--E2E0Z	CHILD ATTENTION DEFICIT DISORDER NOS

drop table if exists #acc_med

SELECT b.snz_uid,
a.[snz_acc_claim_uid]
      ,[acc_med_injury_precedence_nbr]
      ,[acc_med_read_code]
      ,[acc_med_read_code_text]
      ,[acc_med_icd9_code]
      ,[acc_med_icd10_code]
      ,[acc_med_injury_diagnosis_code]
      ,[acc_med_injury_diagnosis_text]
	  into #acc_med
  FROM [IDI_Clean_202210].[acc_clean].[medical_codes] as a,[IDI_Clean_202210].[acc_clean].[claims] as b
    where a.snz_acc_claim_uid=b.snz_acc_claim_uid
and	
	([acc_med_read_code] in (
  'E2E0.',
  'E2E00',
  'E2E01',
  'E2E0Z'
  )
  or 
  [acc_med_icd9_code] in ('314.00','314.01','314.1','314.2','314.8','314.9')
  or [acc_med_icd10_code] in ('F90.0','F90.1','F90.8','F90.9')
  )

  select * from #acc_med

  drop table if exists #acc_ch 

SELECT [snz_uid]
      ,[snz_acc_uid]
      ,[snz_employee_ird_uid]
      ,[acc_cla_accident_date]
      ,[acc_cla_read_code]
      ,[acc_cla_read_code_text]
      ,[acc_cla_ICD9_code]
      ,[acc_cla_ICD10_code]
      ,[acc_cla_primary_injury_site_text]
      ,[acc_cla_primary_diagnosis_text]

	  into #acc_ch
  FROM [IDI_Clean_202210].[acc_clean].[claims_historic]
  where [acc_cla_read_code] in (
  'E2E0.',
  'E2E00',
  'E2E01',
  'E2E0Z'
  )
  or 
  [acc_cla_icd9_code] in ('314.00','314.01','314.1','314.2','314.8','314.9')
  or [acc_cla_icd10_code] in ('F90.0','F90.1','F90.8','F90.9')
 
  select * from #acc_ch

  drop table if exists #acc_c

SELECT [snz_uid]
      ,[snz_acc_uid]
      ,[acc_cla_accident_date]
      ,[acc_cla_read_code]
      ,[acc_cla_read_code_text]
      ,[acc_cla_ICD9_code]
      ,[acc_cla_ICD10_code]
      ,[acc_cla_primary_injury_site_text]
  
	  into #acc_c
  FROM [IDI_Clean_202210].[acc_clean].[claims]
  where [acc_cla_read_code] in (
  'E2E0.',
  'E2E00',
  'E2E01',
  'E2E0Z'
  )
  or 
  [acc_cla_icd9_code] in ('314.00','314.01','314.1','314.2','314.8','314.9')
  or [acc_cla_icd10_code] in ('F90.0','F90.1','F90.8','F90.9')


  --vortioxetine
  --sole indicated 
  --multiple indications

  
--ICD/DSM coding systems lookup
SELECT TOP (1000) [CLINICAL_CODE_SYSTEM]
      ,[CLINICAL_SYSTEM_DESCRIPTION]
      ,[CLINICAL_CODE_TYPE]
      ,[CLINICAL_CODE_TYPE_DESCRIPTION]
      ,[CLINICAL_CODE]
      ,[CLINICAL_CODE_DESCRIPTION]
      ,[BLOCK]
      ,[BLOCK_SHORT_DESCRIPTION]
      ,[BLOCK_LONG_DESCRIPTION]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS_CLIN_DIAG_CODES].[clinical_codes]
where ([CLINICAL_CODE_DESCRIPTION] like '%ADHD%' or [CLINICAL_CODE_DESCRIPTION] like '%deficit%'
or  substring([CLINICAL_CODE],1,3) in ('314','R41','R13','F90') ) and [CLINICAL_CODE_TYPE] in ('A','B','D','V')

--R41840 attention concentraation
--R134 screen for developmental

drop table if exists #gw

SELECT       
b.snz_uid
,a.snz_msd_uid
,[need_type_code] as code
      ,[needs_desc]
      ,[need_category_code]
      ,[needs_cat_desc]
	  ,needs_created_date as date
      --,[education_yn]
      --,[health_yn]
	into #gw
  FROM [IDI_Adhoc].[clean_read_CYF].[cyf_gateway_cli_needs] as a ,idi_clean_202210.security.concordance as b
  --where need_type_code='INT'

--and 
  where a.snz_msd_uid=b.snz_msd_uid 
  --and need_category_code in ('BHSS','MHEA')  
  --ADHD or hyperactivity ,attention problems
  and need_type_code in ('HYP110','ATT228')


--dsm iv 7 31400 31401 3149  
--4.0 mortality

drop table if exists #moh_mort_adhd

SELECT [snz_uid]
      ,'MORT' as source
	  ,datefromparts([moh_mor_death_year_nbr],[moh_mor_death_month_nbr],1) as start_date
	  ,datefromparts([moh_mor_death_year_nbr],[moh_mor_death_month_nbr],1) as end_date
      ,[moh_mort_diag_clinical_code] as code
      ,[moh_mort_diag_clinic_type_code]
      ,[moh_mort_diag_clinic_sys_code]
      ,[moh_mort_diag_diag_type_code]
	  ,'ADHD' as type
	  into #moh_mort_adhd
  FROM [IDI_Clean_202210].[moh_clean].[mortality_diagnosis] as a,[IDI_Clean_202210].[moh_clean].[mortality_registrations] as b
    where a.[snz_dia_death_reg_uid]=b.[snz_dia_death_reg_uid]
and (

(substring([moh_mort_diag_clinical_code],1,5) in ('F900','F901','F908','F909','R134','R418') and [moh_mort_diag_clinic_sys_code]>='10')
or 
(substring([moh_mort_diag_clinical_code],1,5) in ('31400','31401','3141','3142','3148','3149')) and [moh_mort_diag_clinic_sys_code] in ('06','6')
and [moh_mort_diag_clinic_type_code] in ('A','B','V'))



--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MSD incapacitation spells
--nothing for ADHD
--this section not used
--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


  drop table if exists #msd_adhd

select [snz_uid]
      --,code_raw as code
      ,from_date as start_date
      ,to_date as end_date
	  --,'INCP' as source 
	  --,'Intellectual Disability' as type
	  --,'82' as code_sys
into #msd_adhd
	  from
((
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incrsn_code] as code_raw
	  ,'I' as type
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
UNION
(
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incrsn95_1_code] as code_raw
	  ,'1' as type
      --,[msd_incp_incrsn95_1_code]
      --,[msd_incp_incrsn95_2_code]
      --,[msd_incp_incrsn95_3_code]
      --,[msd_incp_incrsn95_4_code]
      --,[msd_incp_incapacity_code]
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
UNION
(
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incrsn95_2_code] as code_raw
	  ,'2' as type
      --,[msd_incp_incrsn95_1_code]
      --,[msd_incp_incrsn95_2_code]
      --,[msd_incp_incrsn95_3_code]
      --,[msd_incp_incrsn95_4_code]
      --,[msd_incp_incapacity_code]
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
UNION
(
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incrsn95_3_code] as code_raw
	  ,'3' as type
      --,[msd_incp_incrsn95_1_code]
      --,[msd_incp_incrsn95_2_code]
      --,[msd_incp_incrsn95_3_code]
      --,[msd_incp_incrsn95_4_code]
      --,[msd_incp_incapacity_code]
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
UNION
(
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incrsn95_4_code] as code_raw
	  ,'4' as type
      --,[msd_incp_incrsn95_1_code]
      --,[msd_incp_incrsn95_2_code]
      --,[msd_incp_incrsn95_3_code]
      --,[msd_incp_incrsn95_4_code]
      --,[msd_incp_incapacity_code]
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
UNION
(
SELECT [snz_uid]
      ,[msd_incp_incp_from_date] as from_date
      ,[msd_incp_incp_to_date] as to_date
      ,[msd_incp_incapacity_code] as code_raw
	  ,'T' as type
      --,[msd_incp_incrsn95_1_code]
      --,[msd_incp_incrsn95_2_code]
      --,[msd_incp_incrsn95_3_code]
      --,[msd_incp_incrsn95_4_code]
      --,[msd_incp_incapacity_code]
  FROM [IDI_Clean_202210].[msd_clean].[msd_incapacity]
)
) as a
where code_raw is not null and code_raw !='000' and code_raw in ('???')

  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --pharms
  --sole and multiple indications
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


  drop table if exists #phh_adhd
  select snz_uid,type,min(start_date) as start_date,max(end_date) as end_date
  into #phh_adhd
  from (
SELECT a.[snz_uid]
      ,[moh_pha_dispensed_date] as start_date
	        ,case when a.[moh_pha_days_supply_nbr] >0 then dateadd(day,a.[moh_pha_days_supply_nbr],[moh_pha_dispensed_date]) 
	  else [moh_pha_dispensed_date]	  end
	  as end_date
   ,case 
   when b.[chemical_ID] in (3887) then 'S' 
   when b.[chemical_ID] in (1389,1809,3735,3880,3935,1578,2367,3750) then 'M' 
   else 'O' end as type
  FROM [IDI_Clean_202210].[moh_clean].[pharmaceutical] as a, 
  [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moh_dim_form_pack_subsidy_code] as b  
  where a.[moh_pha_dim_form_pack_code]=b.[DIM_FORM_PACK_SUBSIDY_KEY] 
  and (CHEMICAL_ID in (1389,1809,3735,3880,3887,3935,1578,2367,3750) or tg_name3='Stimulants/ADHD Treatments')
  ) as a
  group by snz_uid,type

  

/***************************************************************************
--private hospital discharge
*****************************************************************************/

drop table if exists #pri_adhd

SELECT a.[snz_uid]
	--,'PRI' as source
	,cast([moh_pri_evt_start_date] as date) as start_Date
	,cast([moh_pri_evt_end_date] as date) as end_date
	,[moh_pri_diag_sub_sys_code] as code_sys_1
	,[moh_pri_diag_diag_type_code] as code_sys_2
	,[moh_pri_diag_clinic_code] as code
	into #pri_adhd
FROM (
	select *
	from [IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_event]
) as a
	,[IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_diag] as b
where a.[moh_pri_evt_event_id_nbr]=b.[moh_pri_diag_event_id_nbr]
and [moh_pri_diag_sub_sys_code]=[moh_pri_diag_clinic_sys_code]
and (

(substring([moh_pri_diag_clinic_code],1,5) in ('F900','F901','F908','F909','R134','R418') and [moh_pri_diag_sub_sys_code]>='10')
or 
(substring([moh_pri_diag_clinic_code],1,5) in ('31400','31401','3141','3142','3148','3149')) and [moh_pri_diag_sub_sys_code] in ('06','6')
and [moh_pri_diag_diag_type_code] in ('A','B','V'))

/******************************************************************************
Publicly funded hospital discharge
******************************************************************************/
drop table if exists #pub_adhd

SELECT b.[snz_uid]
--	,'PUB' as source
	--TOP (1000) [moh_dia_event_id_nbr]
	--,[moh_dia_clinical_sys_code]
	--,[moh_dia_submitted_system_code]
	--,[moh_dia_diagnosis_type_code]
	--,[moh_dia_diag_sequence_code]
	,[moh_evt_evst_date] as start_date
	,[moh_evt_even_date] as end_date
	,[moh_dia_submitted_system_code] as code_sys_1
	,[moh_dia_diagnosis_type_code] as code_sys_2
	,[moh_dia_clinical_code] as code
	--,[moh_dia_op_date]
	--,[moh_dia_op_flag_ind]
	--,[moh_dia_condition_onset_code]
	--,[snz_moh_uid]
	--,[moh_evt_event_id_nbr]
	into #pub_adhd
FROM [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_diag] as a 
	,[IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_event] as b
where [moh_dia_clinical_sys_code] = [moh_dia_submitted_system_code] 
and [moh_evt_event_id_nbr]=[moh_dia_event_id_nbr] 
and (

(substring([moh_dia_clinical_code],1,5) in ('F900','F901','F908','F909','R134','R418') and [moh_dia_submitted_system_code]>='10')
or 
(substring([moh_dia_clinical_code],1,5) in ('31400','31401','3141','3142','3148','3149')) and [moh_dia_submitted_system_code] in ('06','6')
and [moh_dia_diagnosis_type_code] in ('A','B','V'))


--select code,count(*) from #pub_adhd
--group by code

--PRIMHD and MHINC
--adhd

drop table if exists #mha_adhd
select snz_uid,start_date,end_date, code
into #mha_adhd
from 
(
  SELECT b.snz_uid
      ,[classification_start] as start_date
	  ,[classification_start] as end_date
	  --,[clinical_coding_system_id] as code_sys_1
	  --,diagnosis_type as code_sys_2
      ,[CLINICAL_CODE] as code
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[moh_primhd_mhinc] as a, [IDI_Clean_202210].[security].[concordance] as b
  where a.snz_moh_uid=b.snz_moh_uid 
  and (

(substring([clinical_coding_system_id],1,5) in ('F900','F901','F908','F909','R134','R418') and [clinical_coding_system_id]>='10')
or 
(substring([clinical_coding_system_id],1,5) in ('31400','31401','3149')) and [clinical_coding_system_id] in ('07','7')
and diagnosis_type in ('A','B','V','P','D'))


  UNION ALL

  SELECT snz_uid
  --[snz_moh_uid]
      --,[REFERRAL_ID]
      --,[ORGANISATION_ID]
      --,[CLASSIFICATION_CODE_ID]
      --,[DIAGNOSIS_TYPE]
      ,datefromparts(substring([CLASSIFICATION_START_DATE],7,4),substring([CLASSIFICATION_START_DATE],4,2),substring([CLASSIFICATION_START_DATE],1,2)) as START_DATE
      ,datefromparts(substring([CLASSIFICATION_end_DATE],7,4),substring([CLASSIFICATION_end_DATE],4,2),substring([CLASSIFICATION_end_DATE],1,2)) as end_DATE
      --,[CLINICAL_CODING_SYSTEM_ID]
      ,[CLINICAL_CODE] as code
      --,[CLINICAL_CODE_TYPE]
      --,[DIAGNOSIS_GROUPING_CODE]
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[primhd_diagnoses] as a , [IDI_Clean_202210].[security].[concordance] as b
  where a.snz_moh_uid=b.snz_moh_uid 
    and (

(substring([CLINICAL_CODE],1,5) in ('F900','F901','F908','F909','R134','R418') and [clinical_coding_system_id]>='10')
or 
(substring([CLINICAL_CODE],1,5) in ('31400','31401','3149')) and [clinical_coding_system_id] in ('07','7')
)
) as a


--SOCRATES - NONE
drop table if exists #soc_adhd
--MOH SOCRATES diagnoses 
--code	description
--1201	Attention deficit / hyperactivity, e.g. ADD, ADHD

SELECT b.snz_uid
	  ,min(case when cast(substring([FirstContactDate],1,7) as date) is not null then cast(substring([FirstContactDate],1,7) as date) 
	  when cast(substring([ReferralDate],1,7) as date) is not null then cast(substring([ReferralDate],1,7) as date) 
	  end) as start_date
	  ,min(case when cast(substring([FirstContactDate],1,7) as date) is not null then cast(substring([FirstContactDate],1,7) as date) 
	  when cast(substring([ReferralDate],1,7) as date) is not null then cast(substring([ReferralDate],1,7) as date) 
	  end) as end_date
      ,a.[Code] as code
      ,a.[Description] as description
	  into #soc_adhd
  FROM [IDI_Adhoc].[clean_read_MOH_SOCRATES].moh_disability as a 
  left join [IDI_Clean_202210].[security].[concordance] as b on a.snz_moh_uid=b.snz_moh_uid 
  left join (select distinct snz_moh_uid,[FirstContactDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_needs_assessment] ) as c on a.snz_moh_uid=c.snz_moh_uid 
  left join (select distinct snz_moh_uid,[ReferralDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_referral]) as e on a.snz_moh_uid=e.snz_moh_uid
  where a.snz_moh_uid=c.snz_moh_uid  
  --and a.description like '%ADHD%'
  and a.[code] = '1201'
  group by b.snz_uid,a.code,a.description

 
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --any indication or diagnosis soley for ADHD
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  drop table if exists #adhd

  select snz_uid,min(start_date) as start_date
  ,max(end_date) as end_date, 1 as adhd_diagnosis
  into #adhd
  from 
  (
  select snz_uid,date start_Date,date end_date from #gw  where code in ('ATT228')
    UNION ALL
  select snz_uid,start_Date,end_date from #pri_adhd  where code in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
    UNION ALL
  select snz_uid,start_Date,end_date from #pub_adhd where code in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  UNION ALL
  select snz_uid,start_Date,end_date from #mha_adhd where code in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  --UNION ALL
  --select snz_uid,start_Date,end_date from #irai_adhd
  UNION ALL
  select snz_uid,start_Date,end_date from #soc_adhd
  --UNION ALL
  --select snz_uid,start_Date,end_date from #msd_adhd
  UNION ALL
  select snz_uid,start_Date,end_date from #moh_mort_adhd where code in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  UNION ALL
  select snz_uid,start_Date,end_date from #phh_adhd where type='S'
  ) as a
  group by snz_uid

  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--any non specific codes
--R41840 attention concentraation
--R134 screen for developmental
--HYP110 hyperactive and attention problemss
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --this indicator does not impart any extra sensitivity
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
    drop table if exists #adhd_ns

  select snz_uid,min(start_date) as ns_start_date
  ,max(end_date) as ns_end_date,1 as adhd_ns
  into #adhd_ns
  from 
  (
  --hyperactivity and attention problems, but not specific ADHD diagnosis

  select snz_uid,date start_Date,date end_date,code from #gw  where code in ('HYP110')
UNION
  select snz_uid,start_Date,end_date,code from #pri_adhd  where code not in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
    UNION ALL
  select snz_uid,start_Date,end_date,code from #pub_adhd where code not in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  UNION ALL
  select snz_uid,start_Date,end_date,code from #mha_adhd where code not in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  UNION ALL
  select snz_uid,start_Date,end_date,code from #moh_mort_adhd where not code in ('F900','F901','F908','F909','31400','31401','3141','3142','3148','3149')
  ) as a
  group by snz_uid


  drop table if exists #phh_adhd_2

  select snz_uid,min(start_date) as phh_start_date
  ,max(end_date) as phh_end_date, 1 as adhd_drug
  into #phh_adhd_2
  from 
  (

  select snz_uid,start_Date,end_date from #phh_adhd 
    ) as a
  group by snz_uid

  drop table if exists #b4sc
  
  SELECT [snz_uid]
  ,1 as adhd_b4sc
      ,[moh_bsc_peds_outcome_text]
      ,[moh_bsc_peds_date]
      ,[moh_bsc_peds_pathway_code]
      ,[moh_bsc_peds_shaded_nbr]
      ,[moh_bsc_peds_unshaded_nbr]
      ,[moh_bsc_sdqp_outcome_text]
      ,[moh_bsc_sdqp_date]
      ,[moh_bsc_sdqp_behaviour_nbr]
      ,[moh_bsc_sdqp_conduct_nbr]
      ,[moh_bsc_sdqp_emotional_nbr]
      ,[moh_bsc_sdqp_hyperactive_nbr]
      ,[moh_bsc_sdqp_peer_prob_nbr]
      ,[moh_bsc_sdqt_outcome_text]
      ,[moh_bsc_sdqt_date]
      ,[moh_bsc_sdqt_behaviour_nbr]
      ,[moh_bsc_sdqt_conduct_nbr]
      ,[moh_bsc_sdqt_emotional_nbr]
      ,[moh_bsc_sdqt_hyperactive_nbr]
      ,[moh_bsc_sdqt_peer_prob_nbr]
	  into #b4sc
  FROM [IDI_Clean_202210].[moh_clean].[b4sc]
  where [moh_bsc_sdqt_date] is not null or [moh_bsc_sdqp_date] is not null


  drop table if exists #final_adhd

  select a.snz_uid
  ,b.adhd_diagnosis
  ,c.adhd_drug
  ,d.adhd_ns
  ,e.adhd_b4sc
  ,b.start_date,b.end_date,c.phh_start_date,c.phh_end_date
  ,d.ns_start_date,d.ns_end_date
        ,[moh_bsc_peds_outcome_text]
      ,[moh_bsc_peds_date]
      ,[moh_bsc_peds_pathway_code]
      ,[moh_bsc_peds_shaded_nbr]
      ,[moh_bsc_peds_unshaded_nbr]
      ,[moh_bsc_sdqp_outcome_text]
      ,[moh_bsc_sdqp_date]
      ,[moh_bsc_sdqp_behaviour_nbr]
      ,[moh_bsc_sdqp_conduct_nbr]
      ,[moh_bsc_sdqp_emotional_nbr]
      ,[moh_bsc_sdqp_hyperactive_nbr]
      ,[moh_bsc_sdqp_peer_prob_nbr]
      ,[moh_bsc_sdqt_outcome_text]
      ,[moh_bsc_sdqt_date]
      ,[moh_bsc_sdqt_behaviour_nbr]
      ,[moh_bsc_sdqt_conduct_nbr]
      ,[moh_bsc_sdqt_emotional_nbr]
      ,[moh_bsc_sdqt_hyperactive_nbr]
      ,[moh_bsc_sdqt_peer_prob_nbr]
  into #final_adhd
  from 
  (
  select distinct a.snz_uid from (
  select snz_uid from #adhd
  UNION ALL
  select snz_uid from #phh_adhd_2
  UNION ALL
  select snz_uid from #adhd_ns
  UNION ALL
  select snz_uid from #b4sc
  ) as a 
  ) as a
left join #adhd as b on a.snz_uid=b.snz_uid
left join #phh_adhd_2 as c on a.snz_uid=c.snz_uid
left join #adhd_ns as d on a.snz_uid=d.snz_uid
left join #b4sc as e on a.snz_uid=e.snz_uid
  


  drop table if exists [IDI_Sandpit].[DL-MAA2021-49].[cw_202210_ADHD]
  select a.*,b.snz_birth_year_nbr,b.snz_deceased_year_nbr,case when b.snz_spine_ind>0 then 1 else 0 end as spine,
  case when adhd_diagnosis=1 then 'Diagnosed' 
  when adhd_drug=1 and (moh_bsc_sdqp_hyperactive_nbr>=7 and moh_bsc_sdqt_hyperactive_nbr>=7) then 'Treated OE' 
  when adhd_drug=1 and (moh_bsc_sdqp_hyperactive_nbr>=7 or moh_bsc_sdqt_hyperactive_nbr>=7) then 'Treated OE' 
  when adhd_ns=1 and (moh_bsc_sdqp_hyperactive_nbr>=7 or moh_bsc_sdqt_hyperactive_nbr>=7) then 'Treated OE' 
  when adhd_drug=1 and adhd_ns=1 then 'Treated OE' 
  when adhd_drug=1 then 'Treated NoOE' 
  when (moh_bsc_sdqp_hyperactive_nbr>=7 or moh_bsc_sdqt_hyperactive_nbr>=7) then 'Screened A' 
  else 'Other' end as adhd_category
	into [IDI_Sandpit].[DL-MAA2021-49].[cw_202210_ADHD]
  from #final_adhd as a, idi_clean_202210.data.personal_detail as b
  where a.snz_uid=b.snz_uid 
  --and start_date is not null

  



/* Saving to Alt Ed project sandpit and analysing data */
  select snz_uid,adhd_category,snz_birth_year_nbr 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd]
from  [IDI_Sandpit].[DL-MAA2021-49].[cw_202210_adhd]
where adhd_category in ('Diagnosed','Treated OE','Treated NoOE')



select a.snz_uid, a.snz_birth_year_nbr
,case when b.snz_uid is not null then 1 else 0 end as asd
,case when c.snz_uid is not null then 1 else 0 end as adhd
,c.adhd_diagnosis,c.adhd_b4sc,c.adhd_drug,c.adhd_ns,c.moh_bsc_sdqp_hyperactive_nbr,c.moh_bsc_sdqt_hyperactive_nbr
,d.gw_asd
,d.gw_adhd
,d.gw_hyp_att
into #nd
from 
(
select snz_uid,min(snz_birth_year_nbr) as snz_birth_year_nbr from (
select snz_uid,snz_birth_year_nbr from [IDI_Sandpit].[DL-MAA2021-49].[cw_202210_adhd] where adhd_category not in ('Other')
UNION ALL
select snz_uid,snz_birth_year_nbr from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]
UNION ALL
select snz_uid,null as snz_birth_year_nbr from #gw
) as a group by snz_uid
) as a
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] as b on a.snz_uid=b.snz_uid
left join  (select * from [IDI_Sandpit].[DL-MAA2021-49].[cw_202210_adhd] where adhd_category not in ('Other')) as c on a.snz_uid=c.snz_uid
left join  #gw as d on a.snz_uid=d.snz_uid

select snz_birth_year_nbr,count(*) 
from #nd 
where adhd_diagnosis=1 or adhd_drug=1 or gw_adhd=1 or gw_asd=1 or asd=1 or moh_bsc_sdqp_hyperactive_nbr>=7 or moh_bsc_sdqt_hyperactive_nbr>=7
group by snz_birth_year_nbr
order by snz_birth_year_nbr



select a.birth_year,alted_during,count(*) as pop,sum(case when b.snz_uid is not null then 1 else 0 end ) as nd ,sum(a.adhd) as adhd
,sum(a.asd) as asd
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_output] as a left join #nd as b on a.snz_uid=b.snz_uid
group by a.birth_year,alted_during