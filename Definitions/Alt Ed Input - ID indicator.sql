--Author: C Wright
--Date: 20/10/2021
--Purpose: Intellectual Disability (ID) prevalence

--Indicator: Intellectual Disability (ID)
--Definition: IQ<70 points

--Versons:
--1.0 all diagnostic criteria
--1.1 add ors cognitive criteria
--1.2 add gateway ID assessment - 10/02/2023 

--Alternate defintion for MOH funding purposes: <70 IQ and 2 contributing conditions to impairment and function


--Rationale: ID of IQ<70 points in NZ is thought to be about XX-XXk people
--this ID indicator reaches the appropriate prevalence for younger age groups but not older ones
--it is possible that over the last 15 years the system has been much better at idnetifying people with ID but not so in the past
--and hence the ID prevalence for older people using this method is lower than expected

--If we switched to IDD, then we can use diagnoses that have a high correlation with ID
--for example, XX% of indviduals with DS are thought to have IQ <70 points
--so if we include DS we will capture more older people with low IQ but we will add some that don't
--There is a sens vs spec tradeoff - shifting to an IDD definiton may require a change of language and logic but get us more people who fit the ID definition

--OTHER CONSIDERATIONS 
--- ACUITY : using the ICD and DSM codes it is possible to score people on IQ category
--For example using ICD9:
--317	Mild mental retardation
--3180	Moderate mental retardation
--3181	Severe mental retardation
--3182	Profound mental retardation
--319	Unspecified mental retardation

---Injury: Its possible that ID is in some cases not congenital but due to later insults like TBI
--I have not attempted to flag this possibility using the MOH/ACC/MSD data


--NB : variable name : ORS_cognitive_criteria 1= VH / 2 =H / 3=Moderate
--I have added the ORS criteia for cognition 
--YOU CAN inclde them if you think it is appropriate
--I would say they should definitely be in if its an IDD definition
--but maybe not under a strict ID definition
-- use  : ""where other_sources=1" on the final table

--final table: - see end of file
--select * from #id_demo_ors

--note : ors or nonspecific SOCRATES diagnosis flags are to show whether these children are identified by other sources
--all socrates 1299 are idnetified by other sources
--most ORS flagged cases are not identified by other sources - inclusion would depend on an ID or IDD definition

select top 7 * from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id]


--Coding Systems
--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--Clincial coding of Disaease and mantal health and addications
--code_sys
--06 ICD-9-CM
--07 DSM IV
--10 ICD-10
--11 ICD-10
--12 ICD-10
--13 ICD-10
--14 ICD-10
--15 ICD-10

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
  where 
  --ICD-10
  ((substring([CLINICAL_CODE],1,2) ='F7' 
  --uncertain whether to include these 3 codes - probably not
  or [CLINICAL_CODE] in ('F844','U794','Z810')) and [CLINICAL_CODE_SYSTEM]>=10)
  --ICD-9
  or ([CLINICAL_CODE] in ('317','3180','3181','3182','319','V184','V792') and [CLINICAL_CODE_SYSTEM]=6)



--70 Health Specialty Code - HOspital and non-admmited collections - PRI/ PUB / NAP
SELECT TOP (1000) [HEALTH_SPECIALTY_CODE]
      ,[HEALTH_SPECIALTY_DESCRIPTION]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moh_nmds_health_specialty]
  where [HEALTH_SPECIALTY_CODE]>='D60' and [HEALTH_SPECIALTY_CODE]<='D74'


--71 PRIMHD Team Type Code for intellectual disability and or dual diagnosis teams
SELECT TOP (1000) [TEAM_CODE]
      ,[TEAM_NAME]
      ,[TEAM_TARGET_POPULATION]
      ,[TEAM_TARGET_POPN_DESCRIPTION]
      ,[TEAM_TYPE]
      ,[TEAM_TYPE_DESCRIPTION]
      ,[TEAM_SETTING]
      ,[TEAM_SETTING_DESCRIPTION]
      ,[TEAM_SERVICE_TYPE]
      ,[TEAM_SERVICE_TYPE_DESCRIPTION]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moh_primhd_team_code]
  where team_type='12' or [TEAM_CODE] in ('5994','5995','6007','6745','6207','7281','7382','6140','27','6345','6346','6347','6348','6349','6350','7402','13881','12066','12082','11701','7913','7482','7250','6286','7156','7157','7207','7274','7275','7538')

--80 SOCRATES Diangosis
select distinct code ,description from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_disability] as a 
where Description like '%intell%'

--code	description
--INclude this code
--1208	Intellectual disability (ID), type not specified

--Flag this code
--1299	Other intellectual, learning or developmental disorder (specify)
  --all socrates_1299 coded individuals are identified thourgh other sources as well

--81 IRAI diagnosis
SELECT TOP (1000) [iCode]
      ,[IDI Variable Name]
      ,[Answer_Code]
      ,[Answer]
  FROM [IDI_Metadata_202210].[moh_interrai].[answer19_concord]
  where [IDI Variable Name] like '%intel%'

  SELECT TOP (1000) [iCode]
      ,[IDI Variable Name]
      ,[9_1]
      ,[9_3]
      ,[Acceptable_values]
      ,[iCode_type]
      ,[Question]
      ,[Question_CA]
      ,[Question_HC]
      ,[Question_LTCF]
  FROM [IDI_Metadata_202210].[moh_interrai].[question19_concord]
  where [IDI Variable Name] like '%intel%'

  --variable: moh_irai_res_hist_intellect_ind
  --question: Residential history over last 5 years - Settings for persons with intellectual disability

--82 MSD Incap
SELECT TOP (1000) [Code]
      ,[classification]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[msd_incapacity_reason_code_4]
  where [Code] in ('008','164')

--008	Mental retardation
--164	Mental retardation


--gateway assessment
--gateway assessments
--need_type_code	needs_desc	(No column name)
--INT/DEV	intellectual disability

SELECT       
b.snz_uid
--,a.snz_msd_uid
--,[need_type_code] as code
--      ,[needs_desc]
--      ,[need_category_code]
--      ,[needs_cat_desc]
	  ,needs_created_date as date
      --,[education_yn]
      --,[health_yn]
into #msd_gw
  FROM [IDI_Adhoc].[clean_read_CYF].[cyf_gateway_cli_needs] as a ,idi_clean_202210.security.concordance as b
  where need_type_code='INT'
and 
  a.snz_msd_uid=b.snz_msd_uid



--MOE
--ORS cognitive criteria

--9.1 moderate to high cognitive needs
--1.0 high cognitive needs
--5.0 very high cognitive needs

drop table if exists #moe_ors_cognitive

select snz_uid,min(ORS_cognitive_criteria) as ORS_cognitive_criteria
into #moe_ors_cognitive
from (
SELECT b.snz_uid
--,[snz_moe_uid]
      --,[Exit_Date]
      --,[Exit_Reason_Code]
      ,case 
	  --very high cognitive needs for learning
	  when [Criterion_1]='5.0' or [Criterion_2]='5.0' or [Criterion_3]='5.0' then  1
	  --high cognitive needs for learning
	  when [Criterion_1]='1.0' or [Criterion_2]='1.0' or [Criterion_3]='1.0' then  2
	  --moderate cognitive needs for learning
	  when [Criterion_1] like '%9%' or [Criterion_2] like '%9%' or [Criterion_3] like '%9%' then  3
	  else null end as ORS_cognitive_criteria
      --,[Extration_Date]
  FROM [IDI_Adhoc].[clean_read_MOE].[Student_ORS_Criterion_20171013] as a ,IDI_Clean_202210.security.concordance as b
  where a.[snz_moe_uid]=b.snz_moe_uid 
  ) as a
  where ORS_cognitive_criteria is not null
  group by snz_uid

--1. Mortality collection / ICD9 / ICD10
--2. interrai 
--3. MHINC ICD / DSM Codes
--4. MSD incapacitation codes / med certs from GP
--5. MOH SOCRATES - note the issue with dates
--6. MOH NNPAC  - non admitted patient collection / ED Outpatient and community attendances

--CONDITIONS CORRELATED WITH DEVELOPMENTAL DELAY
--ADHD - diagnosed and putative
--ASD 
--Fetal Alcohol Syndrome
--Fragile-X
--Cerebral palsy
--Spina Bifida
--Developmental Delay
--Kleinfelters Syndromme

--Intellectual Disability

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MOH Mortality collection

drop table if exists #moh_mos
SELECT b.snz_uid,datefromparts([moh_mor_death_year_nbr],[moh_mor_death_month_nbr],1) as date
,a.[snz_dia_death_reg_uid]
      ,[moh_mort_diag_clinical_code] as code
      --,[moh_mort_diag_clinic_type_code]
      ,[moh_mort_diag_clinic_sys_code] as code_sys
      --,[moh_mort_diag_diag_type_code]
	  ,'MOS' as source
,'Intellectual Disability' as type
into #moh_mos
  FROM [IDI_Clean_202210].[moh_clean].[mortality_diagnosis] as a, [IDI_Clean_202210].[moh_clean].[mortality_registrations] as b
  where a.[snz_dia_death_reg_uid]=b.snz_dia_death_reg_uid 
  and (
  substring([moh_mort_diag_clinical_code],1,2)='F7' 
  or substring([moh_mort_diag_clinical_code],1,4)='U794' 
  
  or (substring([moh_mort_diag_clinical_code],1,3) in ('317','318','319') and [moh_mort_diag_clinic_sys_code]='06')
  )


  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--INTERRAI
--'Residential history over last 5 years - Settings for persons with intellectual disability'
--[moh_irai_res_hist_intellect_ind] = 0/1

drop table if exists #moh_irai

SELECT [snz_uid]
,[moh_irai_res_hist_intellect_ind] as code
,[moh_irai_assessment_date] as date

,'81' as code_sys
,'IRAI' as source
,'Intellectual Disability' as type
      --,[moh_irai_cap_cognitive_code]
      --,[moh_irai_res_hist_intellect_ind]
      --,[moh_irai_assessment_date]
      --,[moh_irai_snz_sex_code]
      --,[moh_irai_birth_month_nbr]
      --,[moh_irai_birth_year_nbr]
      --,[moh_irai_death_month_nbr]
      --,[moh_irai_death_year_nbr]
	  into #moh_irai
  FROM [IDI_Clean_202210].[moh_clean].[interrai]
        where 
		--[moh_irai_cap_cognitive_code] >0 or 
		[moh_irai_res_hist_intellect_ind]>0


  -- select top 10 * from #irai_id

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MHINC
  drop table if exists #moh_mhinc_code
  SELECT b.snz_uid
  ,'MHINC' as source
      ,[classification_start] as date
      ,[CLINICAL_CODE] as code
	  ,'Intellectual Disability' as type
	  ,[clinical_coding_system_id] as code_sys
	  into #moh_mhinc_code
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[moh_primhd_mhinc] as a, [IDI_Clean_202210].security.concordance as b
  where 
  ((substring([CLINICAL_CODE],1,2) in ('F7') and [clinical_coding_system_id]>='10') or
  ((substring([CLINICAL_CODE],1,3) in ('317','318','319')) and [clinical_coding_system_id]>='06') or
  ((substring([CLINICAL_CODE],1,3) in ('317','318','319')) and [clinical_coding_system_id]>='07') 
  )
  and a.snz_moh_uid=b.snz_moh_uid
   
  -- select * from #moh_mhinc_code

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MSD - med certificates / incapacitation
('008','164')

  SELECT TOP (1000) [Code]
      ,[classification]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[msd_incapacity_reason_code_4]
  where [classification] ='Mental retardation'
  SELECT TOP (1000) [Code]
      ,[classification]
  FROM [IDI_Metadata].[clean_read_CLASSIFICATIONS].[msd_incapacity_reason_code_4]
  
  drop table if exists #msd_incp_id

select [snz_uid]
      ,code_raw as code
      ,from_date as date
	  ,'INCP' as source 
	  ,'Intellectual Disability' as type
	  ,'82' as code_sys
into #msd_incp_id
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
where code_raw is not null and code_raw !='000' and code_raw in ('008','164')


-- select top 10 * from #msd_incp_id

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MOH SOCRATES 1208 ,1299 
  --all socrates_1299 coded individuals are identified thourgh other sources as well

drop table if exists #moh_soc_id

SELECT b.snz_uid
      ,[Code]
      ,[Description] as text
	  ,case when cast(substring([FirstContactDate],1,7) as date) is not null then cast(substring([FirstContactDate],1,7) as date) 
	  when cast(substring([ReferralDate],1,7) as date) is not null then cast(substring([ReferralDate],1,7) as date) 
	  end as date
	  ,'SOC' as source 
	  ,'Intellectual Disability' as type
	  ,'80' as code_sys
	  into #moh_soc_id
  FROM [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_disability] as a 
  left join [IDI_Clean_202210].security.concordance as b on a.snz_moh_uid=b.snz_moh_uid 
  left join (select distinct snz_moh_uid,[FirstContactDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_needs_assessment] ) as c on a.snz_moh_uid=c.snz_moh_uid 
  left join (select distinct snz_moh_uid,[ReferralDate] from [IDI_Adhoc].[clean_read_MOH_SOCRATES].[moh_referral]) as e on a.snz_moh_uid=e.snz_moh_uid
  where code in ('1208','1299') and a.snz_moh_uid=c.snz_moh_uid 

  -- select top 10 * from #moh_soc_id 
  
  
  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MOH NNPAC  - non admitted patient collection / ED Outpatient and community attendances
drop table if exists #moh_nnp_hs

SELECT [snz_uid]
      ,[moh_nnp_service_date] as date
      ,[moh_nnp_hlth_spc_code] as code
	  ,'NNP' as source 
	  ,'Intellectual Disability' as type
	  ,'70' as code_sys
	  into #moh_nnp_hs
  FROM [IDI_Clean_202210].[moh_clean].[nnpac]
  where [moh_nnp_hlth_spc_code]>='D60' and [moh_nnp_hlth_spc_code]<='D74'

  -- select * from #moh_nnp_hs
  order by snz_uid

  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
drop table if exists #moh_pub_code


-- PUD F7 317,318,319  
SELECT 
--TOP (1000) [moh_dia_event_id_nbr]
--      ,[moh_dia_clinical_sys_code]
--      ,[moh_dia_submitted_system_code]
--      ,[moh_dia_diagnosis_type_code]
--      ,[moh_dia_diag_sequence_code]
      [moh_dia_clinical_code] as code
      --,[moh_dia_op_date]
      --,[moh_dia_op_flag_ind]
      --,[moh_dia_condition_onset_code]
	  ,[snz_uid]
	  ,'PUB' as source
	  ,'Intellectual Disability' as type
      --,[snz_moh_uid]
      --,[moh_evt_event_id_nbr]
	  ,[moh_evt_evst_date] as date
	  ,[moh_dia_submitted_system_code] as code_sys
	  into #moh_pub_code
  FROM [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_diag] as a , [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_event] as b

  where [moh_dia_clinical_sys_code] = [moh_dia_submitted_system_code] and 
  ((substring([moh_dia_clinical_code],1,2) in ('F7') and [moh_dia_submitted_system_code]>='10' and [moh_dia_diagnosis_type_code] in ('A','B')) or
  (substring([moh_dia_clinical_code],1,3) in ('317','318','319') and [moh_dia_submitted_system_code]='06' and [moh_dia_diagnosis_type_code] in ('A','B')))
  and [moh_evt_event_id_nbr]=[moh_dia_event_id_nbr]

 -- select top 10 * from #moh_pub_code

 --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --moh pub hosp / health spec

  drop table if exists #moh_pub_hs
  SELECT [snz_uid]
      ,[moh_evt_evst_date] as date
      ,[moh_evt_hlth_spec_code] as code
	  	  ,'PUB' as source
	  ,'Intellectual Disability' as type
	  ,'70' as code_sys
	  into #moh_pub_hs
  FROM [IDI_Clean_202210].[moh_clean].[pub_fund_hosp_discharges_event]
  where [moh_evt_hlth_spec_code]>='D60' and [moh_evt_hlth_spec_code]<='D74'

  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--PRIMHD
	drop table if exists #moh_primhd_team

	SELECT [snz_uid]
	,'PRIMHD' as source
      --,[snz_moh_uid]
      --,[moh_mhd_organisation_id_code]
      --,[moh_mhd_referral_id_code]
      --,[moh_mhd_referral_from_code]
      --,[moh_mhd_referral_start_date]
      --,[moh_mhd_referral_end_date]
      --,[moh_mhd_referral_end_code]
      --,[moh_mhd_referral_to_code]
      --,[moh_mhd_team_code] as code
      --,[moh_mhd_team_target_popn_code]
      ,[moh_mhd_team_type_code] as code
      --,[moh_mhd_team_setting_code]
      --,[moh_mhd_team_service_type_code]
      --,[moh_mhd_dom_code]
      --,[moh_mhd_dom_org_code]
      --,[moh_mhd_dom_region_text]
      --,[moh_mhd_priority_ethnic_code]
      --,[moh_mhd_activity_id_code]
      --,[moh_mhd_activity_setting_code]
      --,[moh_mhd_activity_type_code]
      --,[moh_mhd_activity_status_code]
      --,[moh_mhd_activity_unit_type_text]
      --,[moh_mhd_activity_unit_count_nbr]
      ,[moh_mhd_activity_start_date] as date
      --,[moh_mhd_activity_end_date]
      --,[moh_mhd_ethnic_snz_code]
      --,[moh_mhd_ethnic_grp1_snz_ind]
      --,[moh_mhd_ethnic_grp2_snz_ind]
      --,[moh_mhd_ethnic_grp3_snz_ind]
      --,[moh_mhd_ethnic_grp4_snz_ind]
      --,[moh_mhd_ethnic_grp5_snz_ind]
      --,[moh_mhd_ethnic_grp6_snz_ind]
	  ,'Intellectual Disability' as type
	  ,'71' as code_sys
	  into #moh_primhd_team
  FROM [IDI_Clean_202210].[moh_clean].[PRIMHD]
  --team type and team code that mention ID
where [moh_mhd_team_type_code]='12' or [moh_mhd_team_code] in (5994,5995,6007,6745,6207,7281,7382,6140,27,6345,6346,6347,6348,6349,6350,7402,13881,12066,
12082,11701,7913,7482,7250,6286,7156,7157,7207,7274,7275,7538)

-- select top 10  * from #moh_primhd_team

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--PRIMHD diangosis codes DSM / ICD
--NB [DIAGNOSIS_TYPE] of P is provisional
drop table if exists #moh_primhd_code

  SELECT b.snz_uid
  
      ,convert(date,[CLASSIFICATION_START_DATE],103)  as date
      --,[CLASSIFICATION_END_DATE]
      --,[CLINICAL_CODING_SYSTEM_ID]
      ,[CLINICAL_CODE] code
      --,[CLINICAL_CODE_TYPE]
      --,[DIAGNOSIS_GROUPING_CODE]
	  	  ,'PRIMHD' as source
	  ,'Intellectual Disability' as type
	  --,[clinical_CODing_system_ID]
	  ,case 
	  when [clinical_CODing_system_ID]=10 then '10'
	  when [clinical_CODing_system_ID]=11 then '11'
	  when [clinical_CODing_system_ID]=12 then '12'
	  when [clinical_CODing_system_ID]=13 then '13'
	  when [clinical_CODing_system_ID]=14 then '14'
	  when [clinical_CODing_system_ID]=7 then '07'
	  when [clinical_CODing_system_ID]=6 then '06'
	  end as code_sys
	  ,[DIAGNOSIS_TYPE] 
	  into #moh_primhd_code
  FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[primhd_diagnoses] as a,[IDI_Clean_202210].security.concordance as b
  where a.[snz_moh_uid]=b.[snz_moh_uid] and
  ((substring([CLINICAL_CODE],1,2)='F7' and [DIAGNOSIS_TYPE] in ('A','B','P') and [clinical_CODing_system_ID]>=10) or 
  (substring([CLINICAL_CODE],1,3) in ('317','318','319') and [DIAGNOSIS_TYPE] in ('A','B','P') and [clinical_CODing_system_ID]=7) or 
  (substring([CLINICAL_CODE],1,3) in ('317','318','319') and [DIAGNOSIS_TYPE] in ('A','B','P') and [clinical_CODing_system_ID]=6))


  --select top 10 * FROM [IDI_Adhoc].[clean_read_MOH_PRIMHD].[primhd_diagnoses]
  --select * from #moh_primhd_code


  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  drop table if exists #moh_pri_hs
  -- moh private hosps / health spec ID codes
  SELECT [snz_uid]
      ,[moh_pri_evt_start_date] as date
      ,[moh_pri_evt_hlthspec_code] as code
	  	  	  ,'PRI' as source
	  ,'Intellectual Disability' as type
	  	  ,'70' as code_sys
into #moh_pri_hs
  FROM [IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_event]
    where [moh_pri_evt_hlthspec_code]>='D60' and [moh_pri_evt_hlthspec_code]<='D74'

	--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	--private hosps / ICD( / ICD10 ID codes
	drop table if exists #moh_pri_code
	SELECT a.[snz_uid]
      ,cast([moh_pri_evt_start_date] as date) as date
      ,[moh_pri_diag_clinic_code] as code
	  	  	  ,'PRI' as source
	  ,'Intellectual Disability' as type
      --,[moh_pri_diag_op_ac_date]
	  	  ,[moh_pri_diag_sub_sys_code] as code_sys
into #moh_pri_code
  FROM [IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_event] as a,[IDI_Clean_202210].[moh_clean].[priv_fund_hosp_discharges_diag] as b
  where a.[moh_pri_evt_event_id_nbr]=b.[moh_pri_diag_event_id_nbr] and 
  (([moh_pri_diag_sub_sys_code]>='10' and [moh_pri_diag_diag_type_code] in ('A','B') and substring([moh_pri_diag_clinic_code],1,2)='F7') or 
  ([moh_pri_diag_sub_sys_code]='06' and [moh_pri_diag_diag_type_code] in ('A','B') and substring([moh_pri_diag_clinic_code],1,3) in ('317','318','319'))
  ) and [moh_pri_diag_clinic_sys_code]=[moh_pri_diag_sub_sys_code]


	--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --FINAL table 
  --although first date is kept in reality the majority of ID is congenital so really ID exists from birth
  
  drop table if exists #id
  
  select snz_uid
  ,min(date) as min_date
  ,max(case when source !='ORS' then 1 else 0 end ) as other_sources
  ,max(case when source ='ORS' then 1 else 0 end ) as ORS_criteria
  --Flag this code
--SOCRATES possibily better under IDD definition - needs to tested what difference it makes
--1299	Other intellectual, learning or developmental disorder (specify)

  ,max(case when code='1299' then 1 else 0 end ) as SOCRATES_1299
  into #id
  from (
  
  --adding ORS criteria
  (select snz_uid,null as code,'ORS' source , null as code_sys, null as date from #moe_ors_cognitive)
  UNION ALL
  
  (select snz_uid,code,source , code_sys, date from #moh_mos)
  UNION ALL
  (select snz_uid,code,source , code_sys, date from #moh_irai)
  UNION ALL
  (select snz_uid,code,source , code_sys, date from #moh_pri_hs)
  UNION ALL
  (select snz_uid,cast(code as varchar),source , code_sys, date from #msd_incp_id)
  UNION
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_soc_id)
  UNION
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_pub_code)
  UNION
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_pri_code)
  UNION
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_primhd_team)
  UNION
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_primhd_code)
  UNION ALL
  (select snz_uid,code,source , code_sys, date from #moh_nnp_hs)
  UNION ALL
  (select snz_uid,code,source , code_sys, date from #moh_pub_hs)
  UNION ALL
  (select snz_uid,cast(code as varchar),source , code_sys, date from #moh_mhinc_code)
  UNION ALL
  (select snz_uid,'INT' ascode,'GW' source ,'GW' code_sys, date from #msd_gw)
  ) as a
  group by snz_uid

   
  select *
  into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id]
  from #id
