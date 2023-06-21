/* 
Alt ed evaluation

This version uses the same logic as the previous version (dividing peoples lives into three periods - prior, during and follow up), but creates four separate tables:
	Variables for matching at age 12 (to predict people who enter AE at age 13)
	Variables for matching at age 13
	Variables for matching at age 14
	Variables for matching at age 15

Note that this code references a few sandpit tables that are created by separate pieces of indicator-specific code. Those pieces of code are saved in the same folder.
Some of these pieces of code rely in turn on the [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] sandpit table, so you might have to run the population construction part of this code first.
The tables referenced in this way (and the corresponding sql code to generate these tables) are:
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_parents]: Alt Ed Input - Parents
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_yju_fgc]: Alt Ed Input - YJ FGC indicator
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id]: Alt Ed Input - ID indicator
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd]: Alt Ed Input - ASD indicator 
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd]: Alt Ed Input - ADHD indicator
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_child]: Alt Ed Input - MHA indicators 
	[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_parents]: Alt Ed Input - MHA indicators

Change log
2023-05-22	AW	Split sql code into parts 1 and 3 and improve documentation for easier reference of external researchers
2023-04-20	AW	Save followup table for total population to sandpit (to provide a comparison for outcomes)
2023-04-18	AW	Add in indicators for disability, head injury, and separating exclusions from suspensions
2023-01-05	AW	Add new robustness method (sm_robust)
2023-03-23	CW	Add date of AE referral to followup tables to facilitate repeating effect size estimates for only the students referred in the first quarter of the year
2023-03-17	AW	Add other learning supports - Behaviour Service, Comms Service, Intensive Wraparound Service and Early Intervention
2023-03-16	AW	Include table for further robustness check (see just after the input tables are saved to sandpit)
2023-03-15	AW	Add other family members involved in alt ed
2023-03-06	AW	Change to age-specific matching tables
2023-03-06	AW	Remove AE referral school aspect of code (because that matching model failed to find common support)
*/


/***********************************************************************************************************
***************************** Define population
***********************************************************************************************************/

/* Select population */
drop table if exists #pop_incompleteparents
select a.snz_uid, b.snz_moe_uid, c.[snz_parent1_uid], c.[snz_parent2_uid]
	, c.snz_birth_year_nbr as birth_year
	, c.snz_deceased_year_nbr as death_year
	, case when c.snz_sex_gender_code=1 then 1 when c.snz_sex_gender_code is null then null else 0 end as male 
	, c.snz_ethnicity_grp1_nbr as eth_european
	, c.snz_ethnicity_grp2_nbr as eth_maori
	, c.snz_ethnicity_grp3_nbr as eth_pacific
	, c.snz_ethnicity_grp4_nbr as eth_asian
	, c.snz_ethnicity_grp5_nbr as eth_melaa
	, c.snz_ethnicity_grp6_nbr as eth_other
	, count(distinct case when a.[apc_age_in_years_nbr]>=0 and a.[apc_age_in_years_nbr]<=12 then [apc_age_in_years_nbr] else null end) as pretreatment_years
	, count(distinct case when a.[apc_age_in_years_nbr]>=13 and a.[apc_age_in_years_nbr]<=15 then [apc_age_in_years_nbr] else null end) as treatment_years
	, count(distinct case when a.[apc_age_in_years_nbr]>=16 and a.[apc_age_in_years_nbr]<=30 then [apc_age_in_years_nbr] else null end) as followup_years
	, max(case when c.snz_parent1_uid is not null and c.snz_parent2_uid is null then 1 else 
		case when c.snz_parent1_uid is null then null else 0 end end) as sole_birth_parent
into #pop_incompleteparents
from [IDI_Clean_202210].[data].[apc_time_series] a 
left join [IDI_Clean_202210].[security].[concordance] b
on a.snz_uid=b.snz_uid
left join [IDI_Clean_202210].[data].[personal_detail] c
on a.snz_uid=c.snz_uid
left join [IDI_Clean_202210].[moe_clean].[student_enrol] d
on a.snz_uid=d.snz_uid
where c.snz_spine_ind=1 -- I think being on spine is a prerequisite of the APC table but just in case
	and c.snz_birth_year_nbr>=1990 
	and c.snz_birth_year_nbr<=2006 -- subset to the birth cohorts who were between the ages of 16-30 in 2022
	and d.snz_uid is not null -- making sure at some point they were enrolled in school
group by a.snz_uid, b.snz_moe_uid, c.snz_birth_year_nbr, c.snz_deceased_year_nbr
	, c.[snz_parent1_uid], c.[snz_parent2_uid], case when c.snz_sex_gender_code=1 then 1 when c.snz_sex_gender_code is null then null else 0 end
	, c.snz_ethnicity_grp1_nbr, c.snz_ethnicity_grp2_nbr, c.snz_ethnicity_grp3_nbr, c.snz_ethnicity_grp4_nbr, c.snz_ethnicity_grp5_nbr, c.snz_ethnicity_grp6_nbr

/* Topping up parent uids using caregiver data 
The APC table only includes parents who are identified on the birth certificate, which means anyone not born in NZ has missing parents
This uses some code written by Craig (saved in the alt ed folder) that produces a table of all parent/caregiver spells (for everyone, ever), which
is saved into the sandpit. This code distinguishes between three "types": 
	B: Birth parent
	1: Primary caregiver
	2: Reported partner of primary caregiver
The code pulls from several different data sources, including DIA, MSD, WFF and MBIE.
The logic I'm using here to top up missing data is to identify cases where either the mother (parent1) or the father (parent2) is missing, then search
in the caregiver table and find the UID of the person of the appropriate gender, using the above priority (birth parent, then primary caregiver, then partner).
This code could also be used to identify instances where caregiver relationships change over the course of the child's life. 
This is an extension we are not currently exploring.*/
drop table if exists #noparents
select * into #noparents from #pop_incompleteparents where [snz_parent1_uid] is null or [snz_parent2_uid] is null

drop table if exists #caregivers
select a.*, case when c.snz_sex_gender_code=2 then 1 when c.snz_sex_gender_code is null then null else 0 end as female
	, row_number() over(partition by child_snz_uid, case when c.snz_sex_gender_code=2 then 1 when c.snz_sex_gender_code is null then null else 0 end 
						order by case when type='B' then 0 else type end, start_date) as rn
into #caregivers
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_parents] a
left join [IDI_Clean_202210].[data].[personal_detail] c
on a.snz_uid=c.snz_uid

drop table if exists #topup_parents
select a.*
	, case when a.snz_parent1_uid is null then b.snz_uid else a.snz_parent1_uid end as new_parent1_uid
	, case when a.snz_parent2_uid is null then c.snz_uid else a.snz_parent2_uid end as new_parent2_uid
into #topup_parents
from #noparents a
left join (select * from #caregivers where rn=1 and female=1) b
on a.snz_uid=b.child_snz_uid
left join (select * from #caregivers where rn=1 and female=0) c
on a.snz_uid=c.child_snz_uid

/* Integrating back into the population table */
drop table if exists #pop
select a.snz_uid, a.snz_moe_uid
	, case when a.snz_parent1_uid is null then b.new_parent1_uid 
		else a.snz_parent1_uid end as snz_parent1_uid
	, case when a.snz_parent2_uid is null then b.new_parent2_uid 
		else a.snz_parent2_uid end as snz_parent2_uid
	, a.birth_year, a.death_year, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other
	, a.pretreatment_years, a.treatment_years, a.followup_years, a.sole_birth_parent
into #pop
from #pop_incompleteparents a
left join #topup_parents b
on a.snz_uid=b.snz_uid

drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop]
select * into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] from #pop

create clustered index pop_index on [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] (snz_uid)

/* Making sure that each row has a unique student */
select count(snz_uid), count(distinct snz_uid), count(snz_moe_uid), count(distinct snz_moe_uid) from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop]

/* Creating table of all unique parent snz_uids - might help with subsetting parent queries later 
Update: Adding in students to this query too so don't need to rerun income tables later */
drop table if exists #parents
select distinct snz_uid 
into #parents
from (
	select snz_uid from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop]
	union
	select snz_parent1_uid as snz_uid from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop]
	union
	select snz_parent2_uid as snz_uid from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop]) a

/* Identify siblings of all learners 
It was identified through the ERO component of the evaluation that many AE learners also had other family members who were concurrently or previously enrolled in AE.
To explore this, we identify all people with familial connections to each learner in our population. 
The precise definition is to identify all people who are birth children of either of the parents for a learner in our population. 
We recognise that this is an incomplete definition of whanau - it will include nuclear family relationships and some aspects of blended families (eg step siblings), but not other whanau relationships. */

/* First identify all children of anyone in the parent table
(Note that the parent table also includes the learners themselves) */
drop table if exists #parent_siblings
select a.snz_uid as parent_snz_uid, b.child_snz_uid
into #parent_siblings
from #parents a
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_parents] b
on a.snz_uid=b.snz_uid

/* Now attach that data back to the learners in the population (via their parents */
drop table if exists #siblings
select distinct snz_uid, child_snz_uid as sibling_snz_uid
into #siblings
from (
	select a.snz_uid, b.child_snz_uid
	from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
	left join #parent_siblings b
	on a.snz_parent1_uid=b.parent_snz_uid
	union 
	select a.snz_uid, c.child_snz_uid
	from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
	left join #parent_siblings c
	on a.snz_parent2_uid=c.parent_snz_uid) a


/***********************************************************************************************************
***************************** Create generic lookup tables
***********************************************************************************************************/

/* Creating table of school spells for everyone in our population */
drop table if exists ##school_spells
select a.*
	, b.providertypeid
	, c.ProviderType
	, b.decilecode
	, b.schoolregionid
	, d.SchoolRegion
	, row_number() over(partition by a.snz_uid order by a.moe_esi_start_date, a.moe_esi_end_date) as rn
	-- Creating an order that helps to identify last enrolment spell per person, used in the follow up analysis
	, row_number() over(partition by a.snz_uid order by case when a.moe_esi_end_date is null then 0 else 1 end, a.moe_esi_end_date desc, case when a.moe_esi_leave_rsn_code is null then 1 else 0 end) as rn_reversed 
	into ##school_spells
	from [IDI_Clean_202210].[moe_clean].[student_enrol] a
	inner join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] p
	on a.snz_uid=p.snz_uid
	left join [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moe_Provider_Profile_20190830] b
	on a.moe_esi_provider_code=b.ProviderNumber
	left join [IDI_Metadata_202210].[moe_school].[provider_type_code] c 
	on b.ProviderTypeId=c.ProviderTypeId
	left join [IDI_Metadata_202210].[moe_school].[sch_region_code] d
	on b.SchoolRegionId=d.SchoolRegionID


/* Comparing cases where students shift from one school to another and identify whether these moves are structural
Structural moves are a movement between schools forced by the structure of the schooling system (eg a student moving between primary and intermediate, or intermediate and secondary school) */
drop table if exists #school_changes
select a.*
	, case when a.moe_esi_provider_code!=b.moe_esi_provider_code then 1 else 0 end as any_move
	, case when a.moe_esi_provider_code!=b.moe_esi_provider_code and (
		(b.providertypeid = 10024 and a.providertypeid in (10023, 10025, 10032, 10029, 10030, 10033) and a.moe_esi_entry_year_lvl_nbr>=7) 
			-- start school is Y1-6, next school is Y1-8/7-8/7-10/7-13/1-13/9-13, moving into Y7 or above
		or (b.providertypeid = 10023 and a.providertypeid in (10032, 10029, 10030, 10033) and a.moe_esi_entry_year_lvl_nbr>=9)
			-- start school is Y1-8, next school is Y7-10/7-13/1-13/9-13, moving into Y9 or above
		or (b.providertypeid = 10025 and a.providertypeid in (10032, 10029, 10030, 10033) and a.moe_esi_entry_year_lvl_nbr>=9)
			-- start school is Y7-8, next school is Y7-10/7-13/1-13/9-13, moving into Y9 or above
		or (b.providertypeid = 10032 and a.providertypeid in (10029, 10030, 10033) and a.moe_esi_entry_year_lvl_nbr>=11)
			-- start school is Y7-10, next school is Y7-13/1-13/9-13, moving into Y11 or above
		or ((b.moe_esi_provider_code in (972,498) or a.providertypeid=10026) and a.providertypeid in (10024, 10023, 10025, 10032, 10029, 10030, 10033))
			-- start school is home school, te kura, or special school, moving into any mainstream school
		or (b.providertypeid in (10024, 10023, 10025, 10032, 10029, 10030, 10033) and (a.moe_esi_provider_code in (972,498) or a.providertypeid=10026))
			-- start school is a mainstream school, moving into home school, te kura, or special school
		) then 1 else 0 end as structural_move
into #school_changes
from ##school_spells a
left join ##school_spells b
on a.snz_uid=b.snz_uid and a.rn-1=b.rn


/* Maori medium participation by age */
drop table if exists ##srr
select snz_moe_uid, year, max(providernumber) as srr_providernumber, max(currentyearlevel) as currentyearlevel, max(mme) as mme
into ##srr
from (
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2012]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2013]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2014]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2015]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2016]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2017]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2018]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2019]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2020]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2021]
	union
	select snz_moe_uid, year(collectiondate) as year, currentyearlevel, providernumber, case when MaoriLanguageLearning in ('F','G','H') then 1 else 0 end as mme
	from [IDI_Adhoc].[clean_read_MOE].[School_Roll_Return_2022]) a
group by snz_moe_uid, year	


/* Creating table of school attendance (Term 2) 
This sums minutes of school attended across the Term 2 dates from 2011-2021. It splits attendance into three categories (all as defined by a table in IDI metadata):
	P: Student is attending school (on-site or off-site)
	J: Student is absent for a justified reason
	U; Student is absent for an unjustified reason
Note there is one code (X; exam leave) that MoE does not include in attendance calculations. It is also not included here.
*/
drop table if exists #attendance
select a.snz_moe_uid
	, year(b.attendancedate) as year
	, sum(case when c.reporting_category = 'P' 
		then b.duration else null end) as attendance_p
	, sum(case when c.reporting_category = 'J'  
		then b.duration else null end) as attendance_j
	, sum(case when c.reporting_category = 'U' 
		then b.duration else null end) as attendance_u
	, sum(case when c.reporting_category in ('P','J','U')  
		then b.duration else null end) as attendance_total
into #attendance
from #pop a
inner join [IDI_Adhoc].[clean_read_MOE].[StudentAttendance] b
on a.snz_moe_uid=b.snz_moe_uid
inner join [IDI_Metadata].[clean_read_CLASSIFICATIONS].[moe_attendance_codes] c
on b.SchoolAttendanceCode=c.school_code
where b.ECEAttendanceCode is null and 
			((b.attendancedate between datefromparts(2011,5,2) and datefromparts(2011,7,15))
			or (b.attendancedate between datefromparts(2012,4,23) and datefromparts(2012,6,29))
			or (b.attendancedate between datefromparts(2013,5,6) and datefromparts(2013,7,12))
			or (b.attendancedate between datefromparts(2014,5,5) and datefromparts(2014,7,4))
			or (b.attendancedate between datefromparts(2015,4,20) and datefromparts(2015,7,3))
			or (b.attendancedate between datefromparts(2016,5,2) and datefromparts(2016,7,8)) 
			or (b.attendancedate between datefromparts(2017,5,1) and datefromparts(2017,7,7)) 
			or (b.attendancedate between datefromparts(2018,4,30) and datefromparts(2018,7,6)) 
			or (b.attendancedate between datefromparts(2019,4,29) and datefromparts(2019,7,5)) 
			or (b.attendancedate between datefromparts(2020,5,17) and datefromparts(2020,7,3))
			or (b.attendancedate between datefromparts(2021,5,3) and datefromparts(2021,7,9)))
group by a.snz_moe_uid, year(b.attendancedate)


/* Creating table of address histories of the population */
drop table if exists #addresses
select a.snz_uid
	, b.ant_notification_date as start_date
	, b.ant_replacement_date as end_date
	, c.NZDep2018
	, row_number() over(partition by a.snz_uid order by b.ant_notification_date) as rn
into #addresses
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
inner join [IDI_Clean_202210].[data].[address_notification] b
on a.snz_uid=b.snz_uid
left join [IDI_Metadata].[clean_read_CLASSIFICATIONS].[DepIndex2018_MB2018] c
on b.ant_meshblock_code=c.MB2018_code

create clustered index address_index on #addresses (snz_uid)


/* Creating income table for parents, by year */
drop table if exists #parent_income
select a.snz_uid
	, b.[inc_cal_yr_sum_year_nbr] as year
	, c.[value] as cpi_2017
	, b.[inc_cal_yr_sum_WAS_tot_amt]
    , b.[inc_cal_yr_sum_WHP_tot_amt]
    , b.[inc_cal_yr_sum_BEN_tot_amt]
    , b.[inc_cal_yr_sum_ACC_tot_amt]
    , b.[inc_cal_yr_sum_PEN_tot_amt]
    , b.[inc_cal_yr_sum_PPL_tot_amt]
    , b.[inc_cal_yr_sum_STU_tot_amt]
    , b.[inc_cal_yr_sum_C01_tot_amt]
    , b.[inc_cal_yr_sum_C02_tot_amt]
    , b.[inc_cal_yr_sum_P01_tot_amt]
    , b.[inc_cal_yr_sum_P02_tot_amt]
    , b.[inc_cal_yr_sum_S01_tot_amt]
    , b.[inc_cal_yr_sum_S02_tot_amt]
    , b.[inc_cal_yr_sum_all_srces_tot_amt]
into #parent_income
from #parents a
inner join [IDI_Clean_202210].[data].[income_cal_yr_summary] b
on a.snz_uid=b.snz_uid
/* Bringing in CPI from a lookup table in the sandpit so can convert income into real figures if necessary */
left join (select *, year(quarter) as year from [IDI_Sandpit].[DL-MAA2021-49].[aw_inflation_index] where month(quarter)=1) c
on b.[inc_cal_yr_sum_year_nbr]=c.year
where b.[inc_cal_yr_sum_year_nbr]>=1990 -- just subsetting this table to not pull data before the first birth cohort, so it's not too large

create clustered index income_index on #parent_income (snz_uid)


/* Create events of attained quals for parents 
Quals are coded as NZQF/ISCED levels (1-10), and are aggregated from Census 2013/2018, tertiary completion data from TEC, 
secondary/tertiary completion data from NZQA, and quals reported by MSD clients. Note that the same quals can be reported in multiple sources. */
drop table if exists ##parent_quals
select snz_uid
	, year
	, source
	, qual
into ##parent_quals
from (
		select a.snz_uid, 2013 as year, 'CEN13' as source
		/* This is probably inefficient coding but seems to categorise as intended. There are two variables that provide the respondent's school and post-school quals. 
		Values greater than 10 in each variable indicate various missing/non-response values, or that the respondent had a (non-specific) qual from overseas.
		The intended logic here is:
			If both qual fields are non-missing, then pick the highest
			If one is non-missing, use that
			Else null. */
		, case when [cen_ind_sndry_scl_qual_code] is not null and [cen_ind_sndry_scl_qual_code]<=4 then 
				case when [cen_ind_post_scl_level_code] is not null and [cen_ind_post_scl_level_code]<=10 then
						case when [cen_ind_sndry_scl_qual_code]>[cen_ind_post_scl_level_code] then [cen_ind_sndry_scl_qual_code] else [cen_ind_post_scl_level_code] end
						else [cen_ind_sndry_scl_qual_code] end
				else case when [cen_ind_post_scl_level_code] is not null and [cen_ind_post_scl_level_code]<=10 then [cen_ind_post_scl_level_code] else null end end as qual
		from #parents a
		left join [IDI_Clean_202210].[cen_clean].[census_individual_2013] b
		on a.snz_uid=b.snz_uid
		union
		select a.snz_uid, 2018 as year, 'CEN18' as source
		, case when cen_ind_standard_hst_qual_code<=10 then cen_ind_standard_hst_qual_code else null end as qual
		from #parents a
		left join [IDI_Clean_202210].[cen_clean].[census_individual_2018] b
		on a.snz_uid=b.snz_uid
		union
		select a.snz_uid, moe_com_year_nbr as year, 'TEC' as source
		, case when moe_com_qual_level_code=99 then null else moe_com_qual_level_code end as qual -- 99 is used as a missing value in this field
		from #parents a
		left join [IDI_Clean_202210].[moe_clean].[completion] b
		on a.snz_uid=b.snz_uid
		union
		select a.snz_uid, moe_sql_attained_year_nbr as year, 'NZQA' as source
		, moe_sql_nqf_level_code as qual
		from #parents a
		left join [IDI_Clean_202210].[moe_clean].[student_qualification] b
		on a.snz_uid=b.snz_uid
		union
		select a.snz_uid, case when moe_itl_end_date is not null then year(moe_itl_end_date) else moe_itl_year_nbr end as year, 'ITO' as source
		, case when [moe_itl_level8_qual_awarded_nbr] > 0 then 8
			when [moe_itl_level7_qual_awarded_nbr] > 0 then 7
			when [moe_itl_level6_qual_awarded_nbr] > 0 then 6
			when [moe_itl_level5_qual_awarded_nbr] > 0 then 5
			when [moe_itl_level4_qual_awarded_nbr] > 0 then 4
			when [moe_itl_level3_qual_awarded_nbr] > 0 then 3
			when [moe_itl_level2_qual_awarded_nbr] > 0 then 2
			when [moe_itl_level1_qual_awarded_nbr] > 0 then 1 else null end as qual
		from #parents a
		left join [IDI_Clean_202210].[moe_clean].[tec_it_learner] b
		on a.snz_uid=b.snz_uid
		union
		select a.snz_uid, year(msd_edh_educ_lvl_start_date) as year, 'MSD' as source
		/* This logic is taken from the MSD code module developed by Michele Morris. I note that it is highly conservative - degrees will be highly underestimated. */
		, case when msd_edh_education_code in ('A' ,'B' ,'I') then 0
			when msd_edh_education_code in ('C', 'J') then 1
			when msd_edh_education_code in ('D','F','K') then 2
			/* 'G' is post secondary school quals so it might be potentially higher than 4 on NQF level.*/
			when msd_edh_education_code in ('E','L','G') then 3
			/* H is degree or prof quals so could be potentially higher than 4 on NQF level.*/
			when msd_edh_education_code in ('H','M') then 4 
			else null end as qual
		from #parents a
		left join [IDI_Clean_202210].[msd_clean].[msd_education_history] b
		on a.snz_uid=b.snz_uid) a


/* Create an indicator for whether a sibling is in AE, by year */
drop table if exists #siblings_alted
select a.snz_uid
	, year(b.moe_inv_start_date) as year
	, max(case when b.snz_uid is not null then 1 else 0 end) as sibling_alted
into #siblings_alted
from #siblings a
left join [IDI_Clean_202210].[moe_clean].[student_interventions] b
on a.sibling_snz_uid=b.snz_uid
where b.moe_inv_intrvtn_code=6
group by a.snz_uid, year(b.moe_inv_start_date)



/***********************************************************************************************************
***************************** Create table for matching as at age 12
***********************************************************************************************************/

/* Creating pre-treatment dataset

This selects all people in the population, and aims to create a table that has one row per person and summarises various data that would then be used for matching.
This table is based on age 0-12, or for parent outcomes, life up until the student was 12.

This table was originally created in one query with a lot of joins and took a long time to run. I have instead split it into 7 subqueries and then joined the resulting tables together.
*/
drop table if exists #pretreatment_12_1
select a.snz_uid
	/* Info about the student */
	, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent
	-- Born in NZ
	, max(case when c.snz_uid is not null then 1 else 0 end) as born_in_nz
	-- Prior data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_prior
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_prior
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_prior
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_prior
	, sum(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_n_prior
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_prior
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_prior
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_prior
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_prior
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_prior
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_prior
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_prior
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_prior
	, max(case when d.moe_inv_intrvtn_code=16 then 1 else 0 end) as rr_prior
	, max(case when d.moe_inv_intrvtn_code=25 then 1 else 0 end) as ors_prior
	, max(case when d.moe_inv_intrvtn_code=48 then 1 else 0 end) as rtlb_prior
	, max(case when d.moe_inv_intrvtn_code=5 then 1 else 0 end) as esol_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_prior
into #pretreatment_12_1
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[dia_clean].[births] c
on a.snz_uid=c.snz_uid
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)<=a.birth_year+12 

group by a.snz_uid, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent


drop table if exists #pretreatment_12_2
select a.snz_uid
	-- OT experiences
	, max(case when e.snz_uid is not null then 1 else 0 end) as ot_report_prior
	, max(case when f.snz_uid is not null then 1 else 0 end) as ot_invest_prior
	, max(case when g.snz_uid is not null then 1 else 0 end) as ot_place_prior
	, max(case when ae.fgc>0 then 1 else 0 end) as yj_fgc_prior
	-- ECE prior participation
	, max(case when h.moe_sed_ece_classification_code is null then null 
		when h.moe_sed_ece_classification_code not in (20630,20637)	then 1 
		else 0 end) as ece_participation 
	-- Maori medium schooling
	, max(ac.mme) as mme
	-- Sibling AE enrolment
	, max(af.sibling_alted) as sibling_alted
into #pretreatment_12_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cyf_clean].[cyf_intakes_event] e
on a.snz_uid=e.snz_uid and year(e.cyf_ine_event_from_datetime)<=a.birth_year+12
left join [IDI_Clean_202210].[cyf_clean].[cyf_investgtns_event] f
on a.snz_uid=f.snz_uid and year(f.cyf_ive_event_from_datetime)<=a.birth_year+12
left join (
	select a.snz_uid, b.cyf_ple_event_from_datetime from #pop a
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_event] b
	on a.snz_uid=b.snz_uid
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_details] c
	on b.snz_composite_event_uid=c.snz_composite_event_uid
	where c.cyf_pld_placement_type_code not in ('ADO') -- filtering out adoptions
	) g
on a.snz_uid=g.snz_uid and year(g.cyf_ple_event_from_datetime)<=a.birth_year+12 
left join [IDI_Clean_202210].[moe_clean].[ece_duration] h
on a.snz_uid=h.snz_uid 
left join ##srr ac
on a.snz_moe_uid=ac.snz_moe_uid and ac.year<=a.birth_year+12
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_yju_fgc] ae
on a.snz_uid=ae.snz_uid and ae.year<=a.birth_year+12
left join #siblings_alted af
on a.snz_uid=af.snz_uid and af.year<=a.birth_year+12
group by a.snz_uid



drop table if exists #pretreatment_12_3
select a.snz_uid
	-- Schools enrolled in
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_prior
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_prior
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_prior
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_prior 
	, max(case when i.rn=1 and i.decilecode != 99 then i.decilecode else null end) as first_school_decile -- 99 is used as a code in cases where schools aren't assigned a decile, so set this to null
	, max(case when i.rn=1 then i.schoolregion else null end) as first_school_region
	, sum(i.any_move) as n_schoolchanges_prior
	, sum(i.structural_move) as n_structuralchanges_prior
	, min(case when i.snz_uid is not null then 0 else 1 end) as missing_schools 
	, max(case when i.rn=1 then i.moe_esi_provider_code else null end) as providerid_firstschool
into #pretreatment_12_3
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)<=a.birth_year+12 
group by a.snz_uid

drop table if exists #pretreatment_12_4
select a.snz_uid
	-- Health: Birth outcomes
	, max(j.moh_matb_birthweight_nbr) as birth_weight
	, max(j.moh_matb_gestational_age_nbr) as birth_gestational_age
	, max(j.moh_matb_apgar_scre_aftr_5mn_nbr) as birth_apgar
	, min(case when j.snz_uid is not null then 0 else 1 end) as missing_birth 
	-- Health: Intellectual disability and neurodiversity
	, max(case when x.snz_uid is not null then 1 else 0 end) as id
	, max(case when y.snz_uid is not null then 1 else 0 end) as asd
	, max(case when z.snz_uid is not null then 1 else 0 end) as adhd
	, max(ad.child_mha_12) as child_mha
	-- Attendance at age 12
	, max(cast(k.attendance_p as float)/cast(k.attendance_total as float)*100) as attendance_12_p
	, max(cast(k.attendance_j as float)/cast(k.attendance_total as float)*100) as attendance_12_j
	, max(cast(k.attendance_u as float)/cast(k.attendance_total as float)*100) as attendance_12_u
	, min(case when k.snz_moe_uid is not null then 0 else 1 end) as missing_attendance
	-- NZDep (for first address) and number of addresses by age 12
	, max(case when l.rn=1 then l.NZDep2018 else null end) as first_nzdep
	, max(l.rn) as num_addresses_prior
	, min(case when l.snz_uid is not null then 0 else 1 end) as missing_address
	/* Info about the student's parents 
	Note here that for convenience I am categorising parent 1 as the mother and parent 2 as the father. 
	This is not strictly true for everyone, but true in the vast majority  of cases */
	-- Parental dummies
	, min(case when a.snz_parent1_uid is not null then 0 else 1 end) as missing_mother
	, min(case when a.snz_parent2_uid is not null then 0 else 1 end) as missing_father
	, max(case when a.snz_parent1_uid is not null and a.snz_parent2_uid is null then 1 else 
		case when a.snz_parent1_uid is null then null else 0 end end) as sole_parent
	, max(case when datediff(year, datefromparts(b.snz_birth_year_nbr,1,1), datefromparts(a.birth_year,1,1)) < 20 then 1 else 0 end) as teen_parent
into #pretreatment_12_4
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moh_clean].[maternity_baby] j
on a.snz_uid=j.snz_uid 
left join #attendance k
on a.snz_moe_uid=k.snz_moe_uid and k.year=a.birth_year+12
left join [IDI_Clean_202210].[data].[personal_detail] b
on a.snz_parent1_uid=b.snz_uid
left join #addresses l
on a.snz_uid=l.snz_uid and year(l.start_date)<=a.birth_year+12 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id] x
on a.snz_uid=x.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] y
on a.snz_uid=y.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd] z
on a.snz_uid=z.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_child] ad
on a.snz_uid=ad.snz_uid
group by a.snz_uid

drop table if exists #pretreatment_12_5
select a.snz_uid
	-- Mother income/benefit
	/* Note that my current benefit definition is based on IR data, not MSD data. This is just for convenience - could change this as an extension.*/
	, max(cast(m.inc_cal_yr_sum_all_srces_tot_amt/m.cpi_2017*1000 as bigint)) as mother_totalinc_age12
	, max(cast(m.inc_cal_yr_sum_BEN_tot_amt/m.cpi_2017*1000 as bigint)) as mother_beninc_age12
	, max(case when m.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as mother_benefit_age12
	, min(case when m.snz_uid is not null then 0 else 1 end) as missing_motherinc
	-- Father income/benefit
	, max(cast(n.inc_cal_yr_sum_all_srces_tot_amt/n.cpi_2017*1000 as bigint)) as father_totalinc_age12
	, max(cast(n.inc_cal_yr_sum_BEN_tot_amt/n.cpi_2017*1000 as bigint)) as father_beninc_age12
	, max(case when n.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as father_benefit_age12
	, min(case when n.snz_uid is not null then 0 else 1 end) as missing_fatherinc
into #pretreatment_12_5
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #parent_income m
on a.snz_parent1_uid=m.snz_uid and m.year=a.birth_year+12
left join #parent_income n
on a.snz_parent2_uid=n.snz_uid and n.year=a.birth_year+12
group by a.snz_uid

drop table if exists #pretreatment_12_6
select a.snz_uid
	-- Mother proceeded against
	, max(case when o.snz_uid is not null then 1 else 0 end) as mother_anyoffence_prior
	, max(case when left(o.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentoffence_prior
	-- Father proceeded against
	, max(case when p.snz_uid is not null then 1 else 0 end) as father_anyoffence_prior
	, max(case when left(p.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentoffence_prior
	-- Mother victim of crime
	, max(case when q.snz_uid is not null then 1 else 0 end) as mother_anyvictim_prior
	, max(case when left(q.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentvictim_prior
	-- Father victim of crime
	, max(case when r.snz_uid is not null then 1 else 0 end) as father_anyvictim_prior
	, max(case when left(r.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentvictim_prior
into #pretreatment_12_6
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] o
on a.snz_parent1_uid=o.snz_uid and o.pol_poo_year_nbr<=a.birth_year+12
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] p
on a.snz_parent2_uid=p.snz_uid and p.pol_poo_year_nbr<=a.birth_year+12
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] q
on a.snz_parent1_uid=q.snz_uid and q.pol_pov_year_nbr<=a.birth_year+12
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] r
on a.snz_parent2_uid=r.snz_uid and r.pol_pov_year_nbr<=a.birth_year+12
group by a.snz_uid

drop table if exists #pretreatment_12_7
select a.snz_uid
	-- Mother corrections sentences
	, max(case when s.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as mother_custodial_prior
	, max(case when s.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as mother_community_prior
	-- Father corrections sentences
	, max(case when t.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as father_custodial_prior
	, max(case when t.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as father_community_prior
	-- Mother highest qual
	, max(u.qual) as mother_qual
	-- Father highest qual
	, max(v.qual) as father_qual
	-- Mother mental health and addiction
	, max(aa.parent1_mh) as mother_mh
	, max(aa.parent1_aod) as mother_aod
	, max(aa.parent1_services) as mother_mhservices
	-- Father mental health and addiction
	, max(aa.parent2_mh) as father_mh
	, max(aa.parent2_aod) as father_aod
	, max(aa.parent2_services) as father_mhservices
into #pretreatment_12_7
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] s
on a.snz_parent1_uid=s.snz_uid and year(s.cor_rommp_period_start_date)<=a.birth_year+12
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] t
on a.snz_parent2_uid=t.snz_uid and year(t.cor_rommp_period_start_date)<=a.birth_year+12
/* Note that for quals, I relaxed the birth year restriction somewhat (up until age 16). 
This was to use census data for more cohorts (because otherwise Census 2013 cannot be used for anyone born before 2001).
The required assumption here is that parent quals are not caused by selection into alt ed. */
left join ##parent_quals u
on a.snz_parent1_uid=u.snz_uid and u.year<=a.birth_year+16
left join ##parent_quals v
on a.snz_parent2_uid=v.snz_uid and v.year<=a.birth_year+16 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_parents] aa
on a.snz_uid=aa.snz_uid
group by a.snz_uid

/* New addition 18/4/2023 - add in disability and head injury */
drop table if exists #pretreatment_12_8
select a.snz_uid
	, max(case when ag.[cen_ind_dffcl_comt_code] in (3,4) or ag.[cen_ind_dffcl_hearing_code] in (3,4) or ag.[cen_ind_dffcl_remembering_code] in (3,4) or
		ag.[cen_ind_dffcl_seeing_code] in (3,4) or ag.[cen_ind_dffcl_walking_code] in (3,4) or ag.[cen_ind_dffcl_washing_code] in (3,4) then 1 else 0 end) as disability_2018
	, max(case when ah.[cen_ind_difficulty_acty1_code]='01' or ah.[cen_ind_difficulty_acty2_code]='01' or ah.[cen_ind_difficulty_acty3_code]='01' or ah.[cen_ind_difficulty_acty4_code]='01' or
		ah.[cen_ind_difficulty_acty5_code]='01' or [cen_ind_difficulty_acty6_code]='01' or [cen_ind_difficulty_acty7_code]='01' then 1 else 0 end) as disability_2013
	, max(case when ai.snz_uid is not null then 1 else 0 end) as head_injury
into #pretreatment_12_8
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cen_clean].[census_individual_2018] ag
on a.snz_uid=ag.snz_uid
left join [IDI_Clean_202210].[cen_clean].[census_individual_2013] ah
on a.snz_uid=ah.snz_uid
left join 
	(select *, year(case when accident_date is null then event_date else accident_date end) as year 
		from [IDI_Sandpit].[DL-MAA2021-49].CW_202210_tbi_codes 
		where code in ('S0406','Q2011','S0001','Q2002','S0116','1802','1803','S640Z','S622Z','S0301'
		,'Q2004','S0103','S0110','S64Z.','S002.','S643.','S003.','S62Z.','S644.','SYU03','S0120','SF03.','S0604','S031.','S0628','S62..','S603.','S00..'
		,'S0605','S0603','S065','S068','S0633','S061','SJ80.','S642.','S098','S0634','S610.','S000.','S0630','S011.','S069','S0638','S066','S097','S0000'
		,'S0623','S03Z.','S001.','S61..','S010.','S099','S01..','S64..','S020','S021','S064','S030.','S04..','S0620','S0622','S6460','S0601','S029','S0621'
		,'S0631','S028','S0...','S092','S0602','S602.','S091','S0600','S044.','S605.','S646.','S090')) ai -- This is a list of codes associated with heightened probability of ID 
on a.snz_uid=ai.snz_uid and ai.year<=a.birth_year+12
group by a.snz_uid


/* Combine pre-treatment tables */
drop table if exists #pretreatment_12_combined
select a.*

	, b.ece_participation 
	, b.ot_report_prior
	, b.ot_invest_prior
	, b.yj_fgc_prior
	, b.ot_place_prior
	, b.mme
	, b.sibling_alted

	, c.activitycentre_prior
	, c.specialschool_prior
	, c.tekura_prior
	, c.homeschool_enrol_prior 
	, c.first_school_decile 
	, case when c.first_school_region is null then 'Unknown' else c.first_school_region end as first_school_region -- R doesn't like nulls in string variables
	, c.n_schoolchanges_prior
	, (c.n_schoolchanges_prior - c.n_structuralchanges_prior) as n_nonstructuralchanges_prior
	, c.missing_schools 
	, c.providerid_firstschool

	, d.birth_weight
	, d.birth_gestational_age
	, d.birth_apgar
	, d.missing_birth 
	, d.id
	, d.asd
	, d.adhd
	, d.child_mha
	, d.attendance_12_p
	, d.attendance_12_j
	, d.attendance_12_u
	, d.missing_attendance

	, d.first_nzdep
	, d.num_addresses_prior
	, d.missing_address
	, d.missing_mother
	, d.missing_father
	, d.teen_parent

	, e.mother_totalinc_age12
	, e.mother_beninc_age12
	, e.mother_benefit_age12
	, e.missing_motherinc
	, e.father_totalinc_age12
	, e.father_beninc_age12
	, e.father_benefit_age12
	, e.missing_fatherinc

	, f.mother_anyoffence_prior
	, f.mother_violentoffence_prior
	, f.father_anyoffence_prior
	, f.father_violentoffence_prior
	, f.mother_anyvictim_prior
	, f.mother_violentvictim_prior
	, f.father_anyvictim_prior
	, f.father_violentvictim_prior

	, g.mother_custodial_prior
	, g.mother_community_prior
	, g.father_custodial_prior
	, g.father_community_prior
	, g.mother_qual
	, g.father_qual
	, g.mother_mh
	, g.mother_aod
	, g.mother_mhservices
	, g.father_mh
	, g.father_aod
	, g.father_mhservices

	, h.disability_2018
	, h.disability_2013
	, case when h.disability_2018=1 or h.disability_2013=1 then 1 else 0 end as disability_combined
	, h.head_injury

into #pretreatment_12_combined
from #pretreatment_12_1 a
left join #pretreatment_12_2 b
on a.snz_uid=b.snz_uid
left join #pretreatment_12_3 c
on a.snz_uid=c.snz_uid
left join #pretreatment_12_4 d
on a.snz_uid=d.snz_uid
left join #pretreatment_12_5 e
on a.snz_uid=e.snz_uid
left join #pretreatment_12_6 f
on a.snz_uid=f.snz_uid
left join #pretreatment_12_7 g
on a.snz_uid=g.snz_uid
left join #pretreatment_12_8 h
on a.snz_uid=h.snz_uid


/* Coverage analysis (dates are birth years):

	Standdown data		complete only			>= 1991			14 years of follow up
	Schools data		complete only			>= 1994			11 years of follow up
	Att serv data		complete only			>= 1995			10 years of follow up
	OT report data		complete only			>= 1996			9 years of follow up
	Offending data		available only			>= 1997			8 years of follow up
	ORS data			available only			>= 1997			8 years of follow up

	Attendance data		available only			>= 1999			6 years of follow up
	Att serv data (UA)	available only			>= 2001			4 years of follow up
	RR data				available only			>= 2001			4 years of follow up
	Parent qual data	complete only			>= 2001			4 years of follow up
	Victim data			available only			>= 2002			3 years of follow up
	ECE data			available only			>= 2003			2 years of follow up
	RTLB data			complete only			>= 2003			2 years of follow up
	Birth data			available only			>= 2003			2 years of follow up

*/


/* Create a table looking at learning support over ages 13-16 (target age for Alt Ed) */
drop table if exists #treatment_12_combined
select a.snz_uid
	-- Data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_during
	, min(case when d.moe_inv_intrvtn_code=6 then year(d.moe_inv_start_date) else null end) as year_alted
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_during
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_during
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_during
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_during
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_during
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_during
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_during
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_during
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_during
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_during
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_during
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_during
into #treatment_12_combined
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)>=a.birth_year+13 and year(d.moe_inv_start_date)<=a.birth_year+16
group by a.snz_uid


/* Adding on a table looking at the enrolment table */
drop table if exists #treatment_12_combined_2
select a.snz_uid
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_during
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_during
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_during
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_during 
into #treatment_12_combined_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)>=a.birth_year+13 and year(i.moe_esi_start_date)<=a.birth_year+16
group by a.snz_uid


/* Create matching table for output
This table:
	Contains all of the variables that will be used for matching
	Imputes missing data where necessary
	Applies selection criteria
	Includes whether the student was referred to alt ed between ages 13-16 (the outcome that the PSM will match on 
*/
drop table if exists #matching_12
select a.snz_uid
	-- Outcome
	, alted_during
	, year_alted
	
	-- Inclusion/exclusion criteria 
	, birth_year
	, death_year
	, pretreatment_years
	, treatment_years
	, followup_years
	, alted_prior
	, homeschool_prior
	, activitycentre_prior
	, specialschool_prior
	, tekura_prior
	, homeschool_during
	, tpu_prior
	, tpu_during
	, activitycentre_during
	, specialschool_during
	, tekura_during
	, homeschool_enrol_during 
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 then 1 else 0 end as altsetting_prior
	, case when activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_during
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 
		or activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_ever

	--Demographics   
	, male
	, eth_european
	, eth_maori
	, eth_pacific
	, eth_asian
	, eth_melaa
	, eth_other
	, case when eth_maori=1 and eth_european=0 and eth_pacific=0 and eth_asian=0 and eth_melaa=0 and eth_other=0 then 1 else 0 end as sole_maori

	-- Missing variables
	, born_in_nz
	, sole_birth_parent
	, missing_address
	, missing_schools
	, missing_mother
	, missing_father
	, missing_motherinc
	, missing_fatherinc

	, ot_report_prior
	, ot_invest_prior
	, yj_fgc_prior
	, ot_place_prior
	, attserv_prior
	, ors_prior
	, esol_prior
	, behavserv_prior
	, commserv_prior
	, earlyint_prior
	, iws_prior
	, behavserv_during
	, commserv_during
	, iws_during
	, mme as mme_prior
	, sibling_alted
	, first_school_region
	, num_addresses_prior
	, teen_parent
	, id
	, asd
	, adhd
	, child_mha
	, mother_totalinc_age12
	, mother_benefit_age12
	, father_totalinc_age12
	, father_benefit_age12
	, mother_anyoffence_prior
	, mother_violentoffence_prior
	, father_anyoffence_prior
	, father_violentoffence_prior
	, mother_custodial_prior
	, mother_community_prior
	, father_custodial_prior
	, father_community_prior
	, case when mother_mh=1 or mother_aod=1 or mother_mhservices=1 then 1 else 0 end as mother_mh -- combining these three variables because they might be conceptually important but were individually non-significant
	, case when father_mh=1 or father_aod=1 or father_mhservices=1 then 1 else 0 end as father_mh 
	, case when mother_qual = 0 then '0 None' 
			when mother_qual in (1,2,3) then '1 Sch'
			when mother_qual in (4,5) then '2 Dip'
			when mother_qual in (6,7) then '3 Deg'
			when mother_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as mother_qual_cat
	, case when father_qual = 0 then '0 None' 
			when father_qual in (1,2,3) then '1 Sch'
			when father_qual in (4,5) then '2 Dip'
			when father_qual in (6,7) then '3 Deg'
			when father_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as father_qual_cat

	-- Top-coding these counts so can use as binary variables
	, case when attserv_n_prior > 2 then 3 else attserv_n_prior end as attserv_n_prior
	, case when standdown_n_prior > 4 then 5 else standdown_n_prior end as standdown_n_prior
	, case when suspension_n_prior > 2 then 3 else suspension_n_prior end as suspension_n_prior
	, case when exclusion_n_prior > 2 then 3 else exclusion_n_prior end as exclusion_n_prior
	, case when n_nonstructuralchanges_prior > 2 then 3 else n_nonstructuralchanges_prior end as n_nonstructuralchanges_prior

	/* In most cases, we want to replace a missing value with a 0, along with a separate binary indicator showing the value is missing.
	However, in some cases, 0 values do not make logical sense (school deciles and NZDep). In these cases set it to 5 */
	, case when first_school_decile is null then 5 else first_school_decile end as first_school_decile
	, case when first_nzdep is null then 5 else first_nzdep end as first_nzdep

	/* Provider ID to provide entity counts */
	, a.providerid_firstschool

	/* Attendance data to check balancing */
	, attendance_12_p
	, attendance_12_j
	, attendance_12_u
	, missing_attendance

	/* New data as of 18/4/2023 */
	, disability_2018
	, disability_2013
	, disability_combined
	, head_injury

into #matching_12
from #pretreatment_12_combined a
left join #treatment_12_combined b
on a.snz_uid=b.snz_uid
left join #treatment_12_combined_2 c
on a.snz_uid=c.snz_uid


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_input]
select * 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_input]
from #matching_12
where alted_prior=0 -- didn't get referred to alted before year turned 13
	and (year_alted is null or year_alted-birth_year<14) -- didn't get referred to alted after year turned 13
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident


/***********************************************************************************************************
***************************** Create table for matching as at age 13
***********************************************************************************************************/

/* Creating pre-treatment dataset

This selects all people in the population, and aims to create a table that has one row per person and summarises various data that would then be used for matching.
This table is based on age 0-13, or for parent outcomes, life up until the student was 13.

This table was originally created in one query with a lot of joins and took a long time to run. I have instead split it into 7 subqueries and then joined the resulting tables together.
*/
drop table if exists #pretreatment_13_1
select a.snz_uid
	/* Info about the student */
	, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent
	-- Born in NZ
	, max(case when c.snz_uid is not null then 1 else 0 end) as born_in_nz
	-- Prior data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_prior
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_prior
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_prior
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_prior
	, sum(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_n_prior
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_prior
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_prior
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_prior
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_prior
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_prior
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_prior
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_prior
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_prior
	, max(case when d.moe_inv_intrvtn_code=16 then 1 else 0 end) as rr_prior
	, max(case when d.moe_inv_intrvtn_code=25 then 1 else 0 end) as ors_prior
	, max(case when d.moe_inv_intrvtn_code=48 then 1 else 0 end) as rtlb_prior
	, max(case when d.moe_inv_intrvtn_code=5 then 1 else 0 end) as esol_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_prior
into #pretreatment_13_1
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[dia_clean].[births] c
on a.snz_uid=c.snz_uid
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)<=a.birth_year+13 

group by a.snz_uid, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent


drop table if exists #pretreatment_13_2
select a.snz_uid
	-- OT experiences
	, max(case when e.snz_uid is not null then 1 else 0 end) as ot_report_prior
	, max(case when f.snz_uid is not null then 1 else 0 end) as ot_invest_prior
	, max(case when g.snz_uid is not null then 1 else 0 end) as ot_place_prior
	, max(case when ae.fgc>0 then 1 else 0 end) as yj_fgc_prior
	-- ECE prior participation
	, max(case when h.moe_sed_ece_classification_code is null then null 
		when h.moe_sed_ece_classification_code not in (20630,20637)	then 1 
		else 0 end) as ece_participation 
	-- Maori medium schooling
	, max(ac.mme) as mme
	-- Sibling AE enrolment
	, max(af.sibling_alted) as sibling_alted
into #pretreatment_13_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cyf_clean].[cyf_intakes_event] e
on a.snz_uid=e.snz_uid and year(e.cyf_ine_event_from_datetime)<=a.birth_year+13
left join [IDI_Clean_202210].[cyf_clean].[cyf_investgtns_event] f
on a.snz_uid=f.snz_uid and year(f.cyf_ive_event_from_datetime)<=a.birth_year+13
left join (
	select a.snz_uid, b.cyf_ple_event_from_datetime from #pop a
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_event] b
	on a.snz_uid=b.snz_uid
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_details] c
	on b.snz_composite_event_uid=c.snz_composite_event_uid
	where c.cyf_pld_placement_type_code not in ('ADO') -- filtering out adoptions
	) g
on a.snz_uid=g.snz_uid and year(g.cyf_ple_event_from_datetime)<=a.birth_year+13 
left join [IDI_Clean_202210].[moe_clean].[ece_duration] h
on a.snz_uid=h.snz_uid 
left join ##srr ac
on a.snz_moe_uid=ac.snz_moe_uid and ac.year<=a.birth_year+13
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_yju_fgc] ae
on a.snz_uid=ae.snz_uid and ae.year<=a.birth_year+13
left join #siblings_alted af
on a.snz_uid=af.snz_uid and af.year<=a.birth_year+13
group by a.snz_uid


drop table if exists #pretreatment_13_3
select a.snz_uid
	-- Schools enrolled in
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_prior
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_prior
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_prior
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_prior 
	, max(case when i.rn=1 and i.decilecode != 99 then i.decilecode else null end) as first_school_decile -- 99 is used as a code in cases where schools aren't assigned a decile, so set this to null
	, max(case when i.rn=1 then i.schoolregion else null end) as first_school_region
	, sum(i.any_move) as n_schoolchanges_prior
	, sum(i.structural_move) as n_structuralchanges_prior
	, min(case when i.snz_uid is not null then 0 else 1 end) as missing_schools 
	, max(case when i.rn=1 then i.moe_esi_provider_code else null end) as providerid_firstschool
into #pretreatment_13_3
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)<=a.birth_year+13 
group by a.snz_uid


drop table if exists #pretreatment_13_4
select a.snz_uid
	-- Health: Birth outcomes
	, max(j.moh_matb_birthweight_nbr) as birth_weight
	, max(j.moh_matb_gestational_age_nbr) as birth_gestational_age
	, max(j.moh_matb_apgar_scre_aftr_5mn_nbr) as birth_apgar
	, min(case when j.snz_uid is not null then 0 else 1 end) as missing_birth 
	-- Health: Intellectual disability and neurodiversity
	, max(case when x.snz_uid is not null then 1 else 0 end) as id
	, max(case when y.snz_uid is not null then 1 else 0 end) as asd
	, max(case when z.snz_uid is not null then 1 else 0 end) as adhd
	, max(ad.child_mha_13) as child_mha
	-- Attendance at age 13
	, max(cast(k.attendance_p as float)/cast(k.attendance_total as float)*100) as attendance_13_p
	, max(cast(k.attendance_j as float)/cast(k.attendance_total as float)*100) as attendance_13_j
	, max(cast(k.attendance_u as float)/cast(k.attendance_total as float)*100) as attendance_13_u
	, min(case when k.snz_moe_uid is not null then 0 else 1 end) as missing_attendance
	-- NZDep (for first address) and number of addresses by age 13
	, max(case when l.rn=1 then l.NZDep2018 else null end) as first_nzdep
	, max(l.rn) as num_addresses_prior
	, min(case when l.snz_uid is not null then 0 else 1 end) as missing_address
	/* Info about the student's parents 
	Note here that for convenience I am categorising parent 1 as the mother and parent 2 as the father. 
	This is not strictly true for everyone, but true in the vast majority of cases */
	-- Parental dummies
	, min(case when a.snz_parent1_uid is not null then 0 else 1 end) as missing_mother
	, min(case when a.snz_parent2_uid is not null then 0 else 1 end) as missing_father
	, max(case when a.snz_parent1_uid is not null and a.snz_parent2_uid is null then 1 else 
		case when a.snz_parent1_uid is null then null else 0 end end) as sole_parent
	, max(case when datediff(year, datefromparts(b.snz_birth_year_nbr,1,1), datefromparts(a.birth_year,1,1)) < 20 then 1 else 0 end) as teen_parent
into #pretreatment_13_4
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moh_clean].[maternity_baby] j
on a.snz_uid=j.snz_uid 
left join #attendance k
on a.snz_moe_uid=k.snz_moe_uid and k.year=a.birth_year+13
left join [IDI_Clean_202210].[data].[personal_detail] b
on a.snz_parent1_uid=b.snz_uid
left join #addresses l
on a.snz_uid=l.snz_uid and year(l.start_date)<=a.birth_year+13 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id] x
on a.snz_uid=x.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] y
on a.snz_uid=y.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd] z
on a.snz_uid=z.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_child] ad
on a.snz_uid=ad.snz_uid
group by a.snz_uid


drop table if exists #pretreatment_13_5
select a.snz_uid
	-- Mother income/benefit
	/* Note that my current benefit definition is based on IR data, not MSD data. This is just for convenience - could change this as an extension.*/
	, max(cast(m.inc_cal_yr_sum_all_srces_tot_amt/m.cpi_2017*1000 as bigint)) as mother_totalinc_age13
	, max(cast(m.inc_cal_yr_sum_BEN_tot_amt/m.cpi_2017*1000 as bigint)) as mother_beninc_age13
	, max(case when m.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as mother_benefit_age13
	, min(case when m.snz_uid is not null then 0 else 1 end) as missing_motherinc
	-- Father income/benefit
	, max(cast(n.inc_cal_yr_sum_all_srces_tot_amt/n.cpi_2017*1000 as bigint)) as father_totalinc_age13
	, max(cast(n.inc_cal_yr_sum_BEN_tot_amt/n.cpi_2017*1000 as bigint)) as father_beninc_age13
	, max(case when n.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as father_benefit_age13
	, min(case when n.snz_uid is not null then 0 else 1 end) as missing_fatherinc
into #pretreatment_13_5
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #parent_income m
on a.snz_parent1_uid=m.snz_uid and m.year=a.birth_year+13
left join #parent_income n
on a.snz_parent2_uid=n.snz_uid and n.year=a.birth_year+13
group by a.snz_uid

drop table if exists #pretreatment_13_6
select a.snz_uid
	-- Mother proceeded against
	, max(case when o.snz_uid is not null then 1 else 0 end) as mother_anyoffence_prior
	, max(case when left(o.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentoffence_prior
	-- Father proceeded against
	, max(case when p.snz_uid is not null then 1 else 0 end) as father_anyoffence_prior
	, max(case when left(p.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentoffence_prior
	-- Mother victim of crime
	, max(case when q.snz_uid is not null then 1 else 0 end) as mother_anyvictim_prior
	, max(case when left(q.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentvictim_prior
	-- Father victim of crime
	, max(case when r.snz_uid is not null then 1 else 0 end) as father_anyvictim_prior
	, max(case when left(r.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentvictim_prior
into #pretreatment_13_6
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] o
on a.snz_parent1_uid=o.snz_uid and o.pol_poo_year_nbr<=a.birth_year+13
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] p
on a.snz_parent2_uid=p.snz_uid and p.pol_poo_year_nbr<=a.birth_year+13
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] q
on a.snz_parent1_uid=q.snz_uid and q.pol_pov_year_nbr<=a.birth_year+13
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] r
on a.snz_parent2_uid=r.snz_uid and r.pol_pov_year_nbr<=a.birth_year+13
group by a.snz_uid


drop table if exists #pretreatment_13_7
select a.snz_uid
	-- Mother corrections sentences
	, max(case when s.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as mother_custodial_prior
	, max(case when s.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as mother_community_prior
	-- Father corrections sentences
	, max(case when t.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as father_custodial_prior
	, max(case when t.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as father_community_prior
	-- Mother highest qual
	, max(u.qual) as mother_qual
	-- Father highest qual
	, max(v.qual) as father_qual
	-- Mother mental health and addiction
	, max(aa.parent1_mh) as mother_mh
	, max(aa.parent1_aod) as mother_aod
	, max(aa.parent1_services) as mother_mhservices
	-- Father mental health and addiction
	, max(aa.parent2_mh) as father_mh
	, max(aa.parent2_aod) as father_aod
	, max(aa.parent2_services) as father_mhservices
into #pretreatment_13_7
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] s
on a.snz_parent1_uid=s.snz_uid and year(s.cor_rommp_period_start_date)<=a.birth_year+13
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] t
on a.snz_parent2_uid=t.snz_uid and year(t.cor_rommp_period_start_date)<=a.birth_year+13
/* Note that for quals, I relaxed the birth year restriction somewhat (up until age 16). 
This was to use census data for more cohorts (because otherwise Census 2013 cannot be used for anyone born before 2001).
The required assumption here is that parent quals are not caused by selection into alt ed. */
left join ##parent_quals u
on a.snz_parent1_uid=u.snz_uid and u.year<=a.birth_year+16
left join ##parent_quals v
on a.snz_parent2_uid=v.snz_uid and v.year<=a.birth_year+16 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_parents] aa
on a.snz_uid=aa.snz_uid
group by a.snz_uid


/* New addition 18/4/2023 - add in disability and head injury */
drop table if exists #pretreatment_13_8
select a.snz_uid
	, max(case when ag.[cen_ind_dffcl_comt_code] in (3,4) or ag.[cen_ind_dffcl_hearing_code] in (3,4) or ag.[cen_ind_dffcl_remembering_code] in (3,4) or
		ag.[cen_ind_dffcl_seeing_code] in (3,4) or ag.[cen_ind_dffcl_walking_code] in (3,4) or ag.[cen_ind_dffcl_washing_code] in (3,4) then 1 else 0 end) as disability_2018
	, max(case when ah.[cen_ind_difficulty_acty1_code]='01' or ah.[cen_ind_difficulty_acty2_code]='01' or ah.[cen_ind_difficulty_acty3_code]='01' or ah.[cen_ind_difficulty_acty4_code]='01' or
		ah.[cen_ind_difficulty_acty5_code]='01' or [cen_ind_difficulty_acty6_code]='01' or [cen_ind_difficulty_acty7_code]='01' then 1 else 0 end) as disability_2013
	, max(case when ai.snz_uid is not null then 1 else 0 end) as head_injury
into #pretreatment_13_8
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cen_clean].[census_individual_2018] ag
on a.snz_uid=ag.snz_uid
left join [IDI_Clean_202210].[cen_clean].[census_individual_2013] ah
on a.snz_uid=ah.snz_uid
left join 
	(select *, year(case when accident_date is null then event_date else accident_date end) as year 
		from [IDI_Sandpit].[DL-MAA2021-49].CW_202210_tbi_codes 
		where code in ('S0406','Q2011','S0001','Q2002','S0116','1802','1803','S640Z','S622Z','S0301'
		,'Q2004','S0103','S0110','S64Z.','S002.','S643.','S003.','S62Z.','S644.','SYU03','S0120','SF03.','S0604','S031.','S0628','S62..','S603.','S00..'
		,'S0605','S0603','S065','S068','S0633','S061','SJ80.','S642.','S098','S0634','S610.','S000.','S0630','S011.','S069','S0638','S066','S097','S0000'
		,'S0623','S03Z.','S001.','S61..','S010.','S099','S01..','S64..','S020','S021','S064','S030.','S04..','S0620','S0622','S6460','S0601','S029','S0621'
		,'S0631','S028','S0...','S092','S0602','S602.','S091','S0600','S044.','S605.','S646.','S090')) ai -- This is a list of codes associated with heightened probability of ID 
on a.snz_uid=ai.snz_uid and ai.year<=a.birth_year+13
group by a.snz_uid



/* Combine pre-treatment tables */
drop table if exists #pretreatment_13_combined
select a.*

	, b.ece_participation 
	, b.ot_report_prior
	, b.ot_invest_prior
	, b.yj_fgc_prior
	, b.ot_place_prior
	, b.mme
	, b.sibling_alted

	, c.activitycentre_prior
	, c.specialschool_prior
	, c.tekura_prior
	, c.homeschool_enrol_prior 
	, c.first_school_decile 
	, case when c.first_school_region is null then 'Unknown' else c.first_school_region end as first_school_region -- R doesn't like nulls in string variables
	, c.n_schoolchanges_prior
	, (c.n_schoolchanges_prior - c.n_structuralchanges_prior) as n_nonstructuralchanges_prior
	, c.missing_schools 
	, c.providerid_firstschool

	, d.birth_weight
	, d.birth_gestational_age
	, d.birth_apgar
	, d.missing_birth 
	, d.id
	, d.asd
	, d.adhd
	, d.child_mha
	, d.attendance_13_p
	, d.attendance_13_j
	, d.attendance_13_u
	, d.missing_attendance

	, d.first_nzdep
	, d.num_addresses_prior
	, d.missing_address
	, d.missing_mother
	, d.missing_father
	, d.teen_parent

	, e.mother_totalinc_age13
	, e.mother_beninc_age13
	, e.mother_benefit_age13
	, e.missing_motherinc
	, e.father_totalinc_age13
	, e.father_beninc_age13
	, e.father_benefit_age13
	, e.missing_fatherinc

	, f.mother_anyoffence_prior
	, f.mother_violentoffence_prior
	, f.father_anyoffence_prior
	, f.father_violentoffence_prior
	, f.mother_anyvictim_prior
	, f.mother_violentvictim_prior
	, f.father_anyvictim_prior
	, f.father_violentvictim_prior

	, g.mother_custodial_prior
	, g.mother_community_prior
	, g.father_custodial_prior
	, g.father_community_prior
	, g.mother_qual
	, g.father_qual
	, g.mother_mh
	, g.mother_aod
	, g.mother_mhservices
	, g.father_mh
	, g.father_aod
	, g.father_mhservices

	, h.disability_2018
	, h.disability_2013
	, case when h.disability_2018=1 or h.disability_2013=1 then 1 else 0 end as disability_combined
	, h.head_injury

into #pretreatment_13_combined
from #pretreatment_13_1 a
left join #pretreatment_13_2 b
on a.snz_uid=b.snz_uid
left join #pretreatment_13_3 c
on a.snz_uid=c.snz_uid
left join #pretreatment_13_4 d
on a.snz_uid=d.snz_uid
left join #pretreatment_13_5 e
on a.snz_uid=e.snz_uid
left join #pretreatment_13_6 f
on a.snz_uid=f.snz_uid
left join #pretreatment_13_7 g
on a.snz_uid=g.snz_uid
left join #pretreatment_13_8 h
on a.snz_uid=h.snz_uid


/* Create a table looking at events over ages 14-16 (target age for Alt Ed) */
drop table if exists #treatment_13_combined
select a.snz_uid
	-- Data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_during
	, min(case when d.moe_inv_intrvtn_code=6 then year(d.moe_inv_start_date) else null end) as year_alted
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_during
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_during
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_during
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_during
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_during
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_during
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_during
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_during
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_during
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_during
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_during
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_during
into #treatment_13_combined
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)>=a.birth_year+14 and year(d.moe_inv_start_date)<=a.birth_year+16
group by a.snz_uid


/* Adding on a table looking at the enrolment table */
drop table if exists #treatment_13_combined_2
select a.snz_uid
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_during
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_during
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_during
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_during 
into #treatment_13_combined_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)>=a.birth_year+14 and year(i.moe_esi_start_date)<=a.birth_year+16
group by a.snz_uid


/* Create matching table for output
This table:
	Contains all of the variables that will be used for matching
	Imputes missing data where necessary
	Applies selection criteria
	Includes whether the student was referred to alt ed between ages 13-16 (the outcome that the PSM will match on 
*/
drop table if exists #matching_13
select a.snz_uid
	-- Outcome
	, alted_during
	, year_alted
	
	-- Inclusion/exclusion criteria 
	, birth_year
	, death_year
	, pretreatment_years
	, treatment_years
	, followup_years
	, alted_prior
	, homeschool_prior
	, activitycentre_prior
	, specialschool_prior
	, tekura_prior
	, homeschool_during
	, tpu_prior
	, tpu_during
	, activitycentre_during
	, specialschool_during
	, tekura_during
	, homeschool_enrol_during 
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 then 1 else 0 end as altsetting_prior
	, case when activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_during
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 
		or activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_ever

	--Demographics   
	, male
	, eth_european
	, eth_maori
	, eth_pacific
	, eth_asian
	, eth_melaa
	, eth_other
	, case when eth_maori=1 and eth_european=0 and eth_pacific=0 and eth_asian=0 and eth_melaa=0 and eth_other=0 then 1 else 0 end as sole_maori

	-- Missing variables
	, born_in_nz
	, sole_birth_parent
	, missing_address
	, missing_schools
	, missing_mother
	, missing_father
	, missing_motherinc
	, missing_fatherinc

	, ot_report_prior
	, ot_invest_prior
	, yj_fgc_prior
	, ot_place_prior
	, attserv_prior
	, ors_prior
	, esol_prior
	, behavserv_prior
	, commserv_prior
	, earlyint_prior
	, iws_prior
	, behavserv_during
	, commserv_during
	, iws_during
	, mme as mme_prior
	, sibling_alted
	, first_school_region
	, num_addresses_prior
	, teen_parent
	, id
	, asd
	, adhd
	, child_mha
	, mother_totalinc_age13
	, mother_benefit_age13
	, father_totalinc_age13
	, father_benefit_age13
	, mother_anyoffence_prior
	, mother_violentoffence_prior
	, father_anyoffence_prior
	, father_violentoffence_prior
	, mother_custodial_prior
	, mother_community_prior
	, father_custodial_prior
	, father_community_prior
	, case when mother_mh=1 or mother_aod=1 or mother_mhservices=1 then 1 else 0 end as mother_mh -- combining these three variables because they might be conceptually important but were individually non-significant
	, case when father_mh=1 or father_aod=1 or father_mhservices=1 then 1 else 0 end as father_mh 
	, case when mother_qual = 0 then '0 None' 
			when mother_qual in (1,2,3) then '1 Sch'
			when mother_qual in (4,5) then '2 Dip'
			when mother_qual in (6,7) then '3 Deg'
			when mother_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as mother_qual_cat
	, case when father_qual = 0 then '0 None' 
			when father_qual in (1,2,3) then '1 Sch'
			when father_qual in (4,5) then '2 Dip'
			when father_qual in (6,7) then '3 Deg'
			when father_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as father_qual_cat

	-- Top-coding these counts so can use as binary variables
	, case when attserv_n_prior > 2 then 3 else attserv_n_prior end as attserv_n_prior
	, case when standdown_n_prior > 4 then 5 else standdown_n_prior end as standdown_n_prior
	, case when suspension_n_prior > 2 then 3 else suspension_n_prior end as suspension_n_prior
	, case when exclusion_n_prior > 2 then 3 else exclusion_n_prior end as exclusion_n_prior
	, case when n_nonstructuralchanges_prior > 2 then 3 else n_nonstructuralchanges_prior end as n_nonstructuralchanges_prior

	/* In most cases, we want to replace a missing value with a 0, along with a separate binary indicator showing the value is missing.
	However, in some cases, 0 values do not make logical sense (school deciles and NZDep). In these cases set it to 5 */
	, case when first_school_decile is null then 5 else first_school_decile end as first_school_decile
	, case when first_nzdep is null then 5 else first_nzdep end as first_nzdep

	/* Provider ID to provide entity counts */
	, a.providerid_firstschool

	/* Attendance data to check balancing */
	, attendance_13_p
	, attendance_13_j
	, attendance_13_u
	, missing_attendance

	/* New data as of 18/4/2023 */
	, disability_2018
	, disability_2013
	, disability_combined
	, head_injury

into #matching_13
from #pretreatment_13_combined a
left join #treatment_13_combined b
on a.snz_uid=b.snz_uid
left join #treatment_13_combined_2 c
on a.snz_uid=c.snz_uid


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_input]
select * 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_input]
from #matching_13
where alted_prior=0 -- didn't get referred to alted before year turned 14
	and (year_alted is null or year_alted-birth_year<15) -- didn't get referred to alted after year turned 14
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident



/***********************************************************************************************************
***************************** Create table for matching as at age 14
***********************************************************************************************************/

/* Creating pre-treatment dataset

This selects all people in the population, and aims to create a table that has one row per person and summarises various data that would then be used for matching.
This table is based on age 0-14, or for parent outcomes, life up until the student was 14.

This table was originally created in one query with a lot of joins and took a long time to run. I have instead split it into 7 subqueries and then joined the resulting tables together.
*/
drop table if exists #pretreatment_14_1
select a.snz_uid
	/* Info about the student */
	, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent
	-- Born in NZ
	, max(case when c.snz_uid is not null then 1 else 0 end) as born_in_nz
	-- Prior data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_prior
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_prior
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_prior
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_prior
	, sum(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_n_prior
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_prior
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_prior
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_prior
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_prior
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_prior
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_prior
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_prior
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_prior
	, max(case when d.moe_inv_intrvtn_code=16 then 1 else 0 end) as rr_prior
	, max(case when d.moe_inv_intrvtn_code=25 then 1 else 0 end) as ors_prior
	, max(case when d.moe_inv_intrvtn_code=48 then 1 else 0 end) as rtlb_prior
	, max(case when d.moe_inv_intrvtn_code=5 then 1 else 0 end) as esol_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_prior
into #pretreatment_14_1
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[dia_clean].[births] c
on a.snz_uid=c.snz_uid
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)<=a.birth_year+14 

group by a.snz_uid, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent


drop table if exists #pretreatment_14_2
select a.snz_uid
	-- OT experiences
	, max(case when e.snz_uid is not null then 1 else 0 end) as ot_report_prior
	, max(case when f.snz_uid is not null then 1 else 0 end) as ot_invest_prior
	, max(case when g.snz_uid is not null then 1 else 0 end) as ot_place_prior
	, max(case when ae.fgc>0 then 1 else 0 end) as yj_fgc_prior
	-- ECE prior participation
	, max(case when h.moe_sed_ece_classification_code is null then null 
		when h.moe_sed_ece_classification_code not in (20630,20637)	then 1 
		else 0 end) as ece_participation 
	-- Maori medium schooling
	, max(ac.mme) as mme
	-- Sibling AE enrolment
	, max(af.sibling_alted) as sibling_alted
into #pretreatment_14_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cyf_clean].[cyf_intakes_event] e
on a.snz_uid=e.snz_uid and year(e.cyf_ine_event_from_datetime)<=a.birth_year+14
left join [IDI_Clean_202210].[cyf_clean].[cyf_investgtns_event] f
on a.snz_uid=f.snz_uid and year(f.cyf_ive_event_from_datetime)<=a.birth_year+14
left join (
	select a.snz_uid, b.cyf_ple_event_from_datetime from #pop a
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_event] b
	on a.snz_uid=b.snz_uid
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_details] c
	on b.snz_composite_event_uid=c.snz_composite_event_uid
	where c.cyf_pld_placement_type_code not in ('ADO') -- filtering out adoptions
	) g
on a.snz_uid=g.snz_uid and year(g.cyf_ple_event_from_datetime)<=a.birth_year+14 
left join [IDI_Clean_202210].[moe_clean].[ece_duration] h
on a.snz_uid=h.snz_uid 
left join ##srr ac
on a.snz_moe_uid=ac.snz_moe_uid and ac.year<=a.birth_year+14
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_yju_fgc] ae
on a.snz_uid=ae.snz_uid and ae.year<=a.birth_year+14
left join #siblings_alted af
on a.snz_uid=af.snz_uid and af.year<=a.birth_year+14
group by a.snz_uid

drop table if exists #pretreatment_14_3
select a.snz_uid
	-- Schools enrolled in
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_prior
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_prior
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_prior
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_prior 
	, max(case when i.rn=1 and i.decilecode != 99 then i.decilecode else null end) as first_school_decile -- 99 is used as a code in cases where schools aren't assigned a decile, so set this to null
	, max(case when i.rn=1 then i.schoolregion else null end) as first_school_region
	, sum(i.any_move) as n_schoolchanges_prior
	, sum(i.structural_move) as n_structuralchanges_prior
	, min(case when i.snz_uid is not null then 0 else 1 end) as missing_schools 
	, max(case when i.rn=1 then i.moe_esi_provider_code else null end) as providerid_firstschool
into #pretreatment_14_3
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)<=a.birth_year+14 
group by a.snz_uid

drop table if exists #pretreatment_14_4
select a.snz_uid
	-- Health: Birth outcomes
	, max(j.moh_matb_birthweight_nbr) as birth_weight
	, max(j.moh_matb_gestational_age_nbr) as birth_gestational_age
	, max(j.moh_matb_apgar_scre_aftr_5mn_nbr) as birth_apgar
	, min(case when j.snz_uid is not null then 0 else 1 end) as missing_birth 
	-- Health: Intellectual disability and neurodiversity
	, max(case when x.snz_uid is not null then 1 else 0 end) as id
	, max(case when y.snz_uid is not null then 1 else 0 end) as asd
	, max(case when z.snz_uid is not null then 1 else 0 end) as adhd
	, max(ad.child_mha_14) as child_mha
	-- Attendance at age 14
	, max(cast(k.attendance_p as float)/cast(k.attendance_total as float)*100) as attendance_14_p
	, max(cast(k.attendance_j as float)/cast(k.attendance_total as float)*100) as attendance_14_j
	, max(cast(k.attendance_u as float)/cast(k.attendance_total as float)*100) as attendance_14_u
	, min(case when k.snz_moe_uid is not null then 0 else 1 end) as missing_attendance
	-- NZDep (for first address) and number of addresses by age 14
	, max(case when l.rn=1 then l.NZDep2018 else null end) as first_nzdep
	, max(l.rn) as num_addresses_prior
	, min(case when l.snz_uid is not null then 0 else 1 end) as missing_address
	/* Info about the student's parents 
	Note here that for convenience I am categorising parent 1 as the mother and parent 2 as the father. 
	This is not strictly true for everyone, but true in the vast majority  of cases */
	-- Parental dummies
	, min(case when a.snz_parent1_uid is not null then 0 else 1 end) as missing_mother
	, min(case when a.snz_parent2_uid is not null then 0 else 1 end) as missing_father
	, max(case when a.snz_parent1_uid is not null and a.snz_parent2_uid is null then 1 else 
		case when a.snz_parent1_uid is null then null else 0 end end) as sole_parent
	, max(case when datediff(year, datefromparts(b.snz_birth_year_nbr,1,1), datefromparts(a.birth_year,1,1)) < 20 then 1 else 0 end) as teen_parent
into #pretreatment_14_4
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moh_clean].[maternity_baby] j
on a.snz_uid=j.snz_uid 
left join #attendance k
on a.snz_moe_uid=k.snz_moe_uid and k.year=a.birth_year+14
left join [IDI_Clean_202210].[data].[personal_detail] b
on a.snz_parent1_uid=b.snz_uid
left join #addresses l
on a.snz_uid=l.snz_uid and year(l.start_date)<=a.birth_year+14 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id] x
on a.snz_uid=x.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] y
on a.snz_uid=y.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd] z
on a.snz_uid=z.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_child] ad
on a.snz_uid=ad.snz_uid
group by a.snz_uid

drop table if exists #pretreatment_14_5
select a.snz_uid
	-- Mother income/benefit
	/* Note that my current benefit definition is based on IR data, not MSD data. This is just for convenience - could change this as an extension.*/
	, max(cast(m.inc_cal_yr_sum_all_srces_tot_amt/m.cpi_2017*1000 as bigint)) as mother_totalinc_age14
	, max(cast(m.inc_cal_yr_sum_BEN_tot_amt/m.cpi_2017*1000 as bigint)) as mother_beninc_age14
	, max(case when m.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as mother_benefit_age14
	, min(case when m.snz_uid is not null then 0 else 1 end) as missing_motherinc
	-- Father income/benefit
	, max(cast(n.inc_cal_yr_sum_all_srces_tot_amt/n.cpi_2017*1000 as bigint)) as father_totalinc_age14
	, max(cast(n.inc_cal_yr_sum_BEN_tot_amt/n.cpi_2017*1000 as bigint)) as father_beninc_age14
	, max(case when n.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as father_benefit_age14
	, min(case when n.snz_uid is not null then 0 else 1 end) as missing_fatherinc
into #pretreatment_14_5
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #parent_income m
on a.snz_parent1_uid=m.snz_uid and m.year=a.birth_year+14
left join #parent_income n
on a.snz_parent2_uid=n.snz_uid and n.year=a.birth_year+14
group by a.snz_uid

drop table if exists #pretreatment_14_6
select a.snz_uid
	-- Mother proceeded against
	, max(case when o.snz_uid is not null then 1 else 0 end) as mother_anyoffence_prior
	, max(case when left(o.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentoffence_prior
	-- Father proceeded against
	, max(case when p.snz_uid is not null then 1 else 0 end) as father_anyoffence_prior
	, max(case when left(p.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentoffence_prior
	-- Mother victim of crime
	, max(case when q.snz_uid is not null then 1 else 0 end) as mother_anyvictim_prior
	, max(case when left(q.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentvictim_prior
	-- Father victim of crime
	, max(case when r.snz_uid is not null then 1 else 0 end) as father_anyvictim_prior
	, max(case when left(r.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentvictim_prior
into #pretreatment_14_6
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] o
on a.snz_parent1_uid=o.snz_uid and o.pol_poo_year_nbr<=a.birth_year+14
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] p
on a.snz_parent2_uid=p.snz_uid and p.pol_poo_year_nbr<=a.birth_year+14
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] q
on a.snz_parent1_uid=q.snz_uid and q.pol_pov_year_nbr<=a.birth_year+14
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] r
on a.snz_parent2_uid=r.snz_uid and r.pol_pov_year_nbr<=a.birth_year+14
group by a.snz_uid

drop table if exists #pretreatment_14_7
select a.snz_uid
	-- Mother corrections sentences
	, max(case when s.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as mother_custodial_prior
	, max(case when s.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as mother_community_prior
	-- Father corrections sentences
	, max(case when t.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as father_custodial_prior
	, max(case when t.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as father_community_prior
	-- Mother highest qual
	, max(u.qual) as mother_qual
	-- Father highest qual
	, max(v.qual) as father_qual
	-- Mother mental health and addiction
	, max(aa.parent1_mh) as mother_mh
	, max(aa.parent1_aod) as mother_aod
	, max(aa.parent1_services) as mother_mhservices
	-- Father mental health and addiction
	, max(aa.parent2_mh) as father_mh
	, max(aa.parent2_aod) as father_aod
	, max(aa.parent2_services) as father_mhservices
into #pretreatment_14_7
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] s
on a.snz_parent1_uid=s.snz_uid and year(s.cor_rommp_period_start_date)<=a.birth_year+14
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] t
on a.snz_parent2_uid=t.snz_uid and year(t.cor_rommp_period_start_date)<=a.birth_year+14
/* Note that for quals, I relaxed the birth year restriction somewhat (up until age 16). 
This was to use census data for more cohorts (because otherwise Census 2013 cannot be used for anyone born before 2001).
The required assumption here is that parent quals are not caused by selection into alt ed. */
left join ##parent_quals u
on a.snz_parent1_uid=u.snz_uid and u.year<=a.birth_year+16
left join ##parent_quals v
on a.snz_parent2_uid=v.snz_uid and v.year<=a.birth_year+16 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_parents] aa
on a.snz_uid=aa.snz_uid
group by a.snz_uid


/* New addition 18/4/2023 - add in disability and head injury */
drop table if exists #pretreatment_14_8
select a.snz_uid
	, max(case when ag.[cen_ind_dffcl_comt_code] in (3,4) or ag.[cen_ind_dffcl_hearing_code] in (3,4) or ag.[cen_ind_dffcl_remembering_code] in (3,4) or
		ag.[cen_ind_dffcl_seeing_code] in (3,4) or ag.[cen_ind_dffcl_walking_code] in (3,4) or ag.[cen_ind_dffcl_washing_code] in (3,4) then 1 else 0 end) as disability_2018
	, max(case when ah.[cen_ind_difficulty_acty1_code]='01' or ah.[cen_ind_difficulty_acty2_code]='01' or ah.[cen_ind_difficulty_acty3_code]='01' or ah.[cen_ind_difficulty_acty4_code]='01' or
		ah.[cen_ind_difficulty_acty5_code]='01' or [cen_ind_difficulty_acty6_code]='01' or [cen_ind_difficulty_acty7_code]='01' then 1 else 0 end) as disability_2013
	, max(case when ai.snz_uid is not null then 1 else 0 end) as head_injury
into #pretreatment_14_8
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cen_clean].[census_individual_2018] ag
on a.snz_uid=ag.snz_uid
left join [IDI_Clean_202210].[cen_clean].[census_individual_2013] ah
on a.snz_uid=ah.snz_uid
left join 
	(select *, year(case when accident_date is null then event_date else accident_date end) as year 
		from [IDI_Sandpit].[DL-MAA2021-49].CW_202210_tbi_codes 
		where code in ('S0406','Q2011','S0001','Q2002','S0116','1802','1803','S640Z','S622Z','S0301'
		,'Q2004','S0103','S0110','S64Z.','S002.','S643.','S003.','S62Z.','S644.','SYU03','S0120','SF03.','S0604','S031.','S0628','S62..','S603.','S00..'
		,'S0605','S0603','S065','S068','S0633','S061','SJ80.','S642.','S098','S0634','S610.','S000.','S0630','S011.','S069','S0638','S066','S097','S0000'
		,'S0623','S03Z.','S001.','S61..','S010.','S099','S01..','S64..','S020','S021','S064','S030.','S04..','S0620','S0622','S6460','S0601','S029','S0621'
		,'S0631','S028','S0...','S092','S0602','S602.','S091','S0600','S044.','S605.','S646.','S090')) ai  -- This is a list of codes associated with heightened probability of ID
on a.snz_uid=ai.snz_uid and ai.year<=a.birth_year+14
group by a.snz_uid


/* Combine pre-treatment tables */
drop table if exists #pretreatment_14_combined
select a.*

	, b.ece_participation 
	, b.ot_report_prior
	, b.ot_invest_prior
	, b.yj_fgc_prior
	, b.ot_place_prior
	, b.mme
	, b.sibling_alted

	, c.activitycentre_prior
	, c.specialschool_prior
	, c.tekura_prior
	, c.homeschool_enrol_prior 
	, c.first_school_decile 
	, case when c.first_school_region is null then 'Unknown' else c.first_school_region end as first_school_region -- R doesn't like nulls in string variables
	, c.n_schoolchanges_prior
	, (c.n_schoolchanges_prior - c.n_structuralchanges_prior) as n_nonstructuralchanges_prior
	, c.missing_schools 
	, c.providerid_firstschool

	, d.birth_weight
	, d.birth_gestational_age
	, d.birth_apgar
	, d.missing_birth 
	, d.id
	, d.asd
	, d.adhd
	, d.child_mha
	, d.attendance_14_p
	, d.attendance_14_j
	, d.attendance_14_u
	, d.missing_attendance

	, d.first_nzdep
	, d.num_addresses_prior
	, d.missing_address
	, d.missing_mother
	, d.missing_father
	, d.teen_parent

	, e.mother_totalinc_age14
	, e.mother_beninc_age14
	, e.mother_benefit_age14
	, e.missing_motherinc
	, e.father_totalinc_age14
	, e.father_beninc_age14
	, e.father_benefit_age14
	, e.missing_fatherinc

	, f.mother_anyoffence_prior
	, f.mother_violentoffence_prior
	, f.father_anyoffence_prior
	, f.father_violentoffence_prior
	, f.mother_anyvictim_prior
	, f.mother_violentvictim_prior
	, f.father_anyvictim_prior
	, f.father_violentvictim_prior

	, g.mother_custodial_prior
	, g.mother_community_prior
	, g.father_custodial_prior
	, g.father_community_prior
	, g.mother_qual
	, g.father_qual
	, g.mother_mh
	, g.mother_aod
	, g.mother_mhservices
	, g.father_mh
	, g.father_aod
	, g.father_mhservices

	, h.disability_2018
	, h.disability_2013
	, case when h.disability_2018=1 or h.disability_2013=1 then 1 else 0 end as disability_combined
	, h.head_injury

into #pretreatment_14_combined
from #pretreatment_14_1 a
left join #pretreatment_14_2 b
on a.snz_uid=b.snz_uid
left join #pretreatment_14_3 c
on a.snz_uid=c.snz_uid
left join #pretreatment_14_4 d
on a.snz_uid=d.snz_uid
left join #pretreatment_14_5 e
on a.snz_uid=e.snz_uid
left join #pretreatment_14_6 f
on a.snz_uid=f.snz_uid
left join #pretreatment_14_7 g
on a.snz_uid=g.snz_uid
left join #pretreatment_14_8 h
on a.snz_uid=h.snz_uid


/* Create a table looking at events over ages 15-16 (target age for Alt Ed) */
drop table if exists #treatment_14_combined
select a.snz_uid
	-- Data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_during
	, min(case when d.moe_inv_intrvtn_code=6 then year(d.moe_inv_start_date) else null end) as year_alted
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_during
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_during
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_during
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_during
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_during
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_during
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_during
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_during
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_during
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_during
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_during
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_during
into #treatment_14_combined
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)>=a.birth_year+15 and year(d.moe_inv_start_date)<=a.birth_year+16
group by a.snz_uid


/* Adding on a table looking at the enrolment table */
drop table if exists #treatment_14_combined_2
select a.snz_uid
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_during
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_during
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_during
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_during 
into #treatment_14_combined_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)>=a.birth_year+15 and year(i.moe_esi_start_date)<=a.birth_year+16
group by a.snz_uid


/* Create matching table for output
This table:
	Contains all of the variables that will be used for matching
	Imputes missing data where necessary
	Applies selection criteria
	Includes whether the student was referred to alt ed between ages 13-16 (the outcome that the PSM will match on 
*/
drop table if exists #matching_14
select a.snz_uid
	-- Outcome
	, alted_during
	, year_alted
	
	-- Inclusion/exclusion criteria 
	, birth_year
	, death_year
	, pretreatment_years
	, treatment_years
	, followup_years
	, alted_prior
	, homeschool_prior
	, activitycentre_prior
	, specialschool_prior
	, tekura_prior
	, homeschool_during
	, tpu_prior
	, tpu_during
	, activitycentre_during
	, specialschool_during
	, tekura_during
	, homeschool_enrol_during 
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 then 1 else 0 end as altsetting_prior
	, case when activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_during
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 
		or activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_ever

	--Demographics   
	, male
	, eth_european
	, eth_maori
	, eth_pacific
	, eth_asian
	, eth_melaa
	, eth_other
	, case when eth_maori=1 and eth_european=0 and eth_pacific=0 and eth_asian=0 and eth_melaa=0 and eth_other=0 then 1 else 0 end as sole_maori

	-- Missing variables
	, born_in_nz
	, sole_birth_parent
	, missing_address
	, missing_schools
	, missing_mother
	, missing_father
	, missing_motherinc
	, missing_fatherinc

	, ot_report_prior
	, ot_invest_prior
	, yj_fgc_prior
	, ot_place_prior
	, attserv_prior
	, ors_prior
	, esol_prior
	, behavserv_prior
	, commserv_prior
	, earlyint_prior
	, iws_prior
	, behavserv_during
	, commserv_during
	, iws_during
	, mme as mme_prior
	, sibling_alted
	, first_school_region
	, num_addresses_prior
	, teen_parent
	, id
	, asd
	, adhd
	, child_mha
	, mother_totalinc_age14
	, mother_benefit_age14
	, father_totalinc_age14
	, father_benefit_age14
	, mother_anyoffence_prior
	, mother_violentoffence_prior
	, father_anyoffence_prior
	, father_violentoffence_prior
	, mother_custodial_prior
	, mother_community_prior
	, father_custodial_prior
	, father_community_prior
	, case when mother_mh=1 or mother_aod=1 or mother_mhservices=1 then 1 else 0 end as mother_mh -- combining these three variables because they might be conceptually important but were individually non-significant
	, case when father_mh=1 or father_aod=1 or father_mhservices=1 then 1 else 0 end as father_mh 
	, case when mother_qual = 0 then '0 None' 
			when mother_qual in (1,2,3) then '1 Sch'
			when mother_qual in (4,5) then '2 Dip'
			when mother_qual in (6,7) then '3 Deg'
			when mother_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as mother_qual_cat
	, case when father_qual = 0 then '0 None' 
			when father_qual in (1,2,3) then '1 Sch'
			when father_qual in (4,5) then '2 Dip'
			when father_qual in (6,7) then '3 Deg'
			when father_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as father_qual_cat

	-- Top-coding these counts so can use as binary variables
	, case when attserv_n_prior > 2 then 3 else attserv_n_prior end as attserv_n_prior
	, case when standdown_n_prior > 4 then 5 else standdown_n_prior end as standdown_n_prior
	, case when suspension_n_prior > 2 then 3 else suspension_n_prior end as suspension_n_prior
	, case when exclusion_n_prior > 2 then 3 else exclusion_n_prior end as exclusion_n_prior
	, case when n_nonstructuralchanges_prior > 2 then 3 else n_nonstructuralchanges_prior end as n_nonstructuralchanges_prior

	/* In most cases, we want to replace a missing value with a 0, along with a separate binary indicator showing the value is missing.
	However, in some cases, 0 values do not make logical sense (school deciles and NZDep). In these cases set it to 5 */
	, case when first_school_decile is null then 5 else first_school_decile end as first_school_decile
	, case when first_nzdep is null then 5 else first_nzdep end as first_nzdep

	/* Provider ID to provide entity counts */
	, a.providerid_firstschool

	/* Attendance data to check balancing */
	, attendance_14_p
	, attendance_14_j
	, attendance_14_u
	, missing_attendance

	/* New data as of 18/4/2023 */
	, disability_2018
	, disability_2013
	, disability_combined
	, head_injury

into #matching_14
from #pretreatment_14_combined a
left join #treatment_14_combined b
on a.snz_uid=b.snz_uid
left join #treatment_14_combined_2 c
on a.snz_uid=c.snz_uid


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_input]
select * 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_input]
from #matching_14
where alted_prior=0 -- didn't get referred to alted before year turned 14
	and (year_alted is null or year_alted-birth_year<16) -- didn't get referred to alted after year turned 15
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident



/***********************************************************************************************************
***************************** Create table for matching as at age 15
***********************************************************************************************************/

/* Creating pre-treatment dataset

This selects all people in the population, and aims to create a table that has one row per person and summarises various data that would then be used for matching.
This table is based on age 0-15, or for parent outcomes, life up until the student was 15.

This table was originally created in one query with a lot of joins and took a long time to run. I have instead split it into 7 subqueries and then joined the resulting tables together.
*/
drop table if exists #pretreatment_15_1
select a.snz_uid
	/* Info about the student */
	, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent
	-- Born in NZ
	, max(case when c.snz_uid is not null then 1 else 0 end) as born_in_nz
	-- Prior data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_prior
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_prior
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_prior
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_prior
	, max(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_prior
	, sum(case when d.moe_inv_intrvtn_code=7 and d.moe_inv_standwn_susp_type_code in (5,6,7,13) then 1 else 0 end) as exclusion_n_prior
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_prior
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_prior
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_prior
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_prior
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_prior
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_prior
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_prior
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_prior
	, max(case when d.moe_inv_intrvtn_code=16 then 1 else 0 end) as rr_prior
	, max(case when d.moe_inv_intrvtn_code=25 then 1 else 0 end) as ors_prior
	, max(case when d.moe_inv_intrvtn_code=48 then 1 else 0 end) as rtlb_prior
	, max(case when d.moe_inv_intrvtn_code=5 then 1 else 0 end) as esol_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_prior
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_prior
into #pretreatment_15_1
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[dia_clean].[births] c
on a.snz_uid=c.snz_uid
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)<=a.birth_year+15 

group by a.snz_uid, a.snz_moe_uid, a.snz_parent1_uid, a.snz_parent2_uid, a.birth_year, a.death_year, a.pretreatment_years, a.treatment_years, a.followup_years
	, a.male, a.eth_european, a.eth_maori, a.eth_pacific, a.eth_asian, a.eth_melaa, a.eth_other, a.sole_birth_parent


drop table if exists #pretreatment_15_2
select a.snz_uid
	-- OT experiences
	, max(case when e.snz_uid is not null then 1 else 0 end) as ot_report_prior
	, max(case when f.snz_uid is not null then 1 else 0 end) as ot_invest_prior
	, max(case when g.snz_uid is not null then 1 else 0 end) as ot_place_prior
	, max(case when ae.fgc>0 then 1 else 0 end) as yj_fgc_prior
	-- ECE prior participation
	, max(case when h.moe_sed_ece_classification_code is null then null 
		when h.moe_sed_ece_classification_code not in (20630,20637)	then 1 
		else 0 end) as ece_participation 
	-- Maori medium schooling
	, max(ac.mme) as mme
	-- Sibling AE enrolment
	, max(af.sibling_alted) as sibling_alted
into #pretreatment_15_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cyf_clean].[cyf_intakes_event] e
on a.snz_uid=e.snz_uid and year(e.cyf_ine_event_from_datetime)<=a.birth_year+15
left join [IDI_Clean_202210].[cyf_clean].[cyf_investgtns_event] f
on a.snz_uid=f.snz_uid and year(f.cyf_ive_event_from_datetime)<=a.birth_year+15
left join (
	select a.snz_uid, b.cyf_ple_event_from_datetime from #pop a
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_event] b
	on a.snz_uid=b.snz_uid
	inner join [IDI_Clean_202210].[cyf_clean].[cyf_placements_details] c
	on b.snz_composite_event_uid=c.snz_composite_event_uid
	where c.cyf_pld_placement_type_code not in ('ADO') -- filtering out adoptions
	) g
on a.snz_uid=g.snz_uid and year(g.cyf_ple_event_from_datetime)<=a.birth_year+15 
left join [IDI_Clean_202210].[moe_clean].[ece_duration] h
on a.snz_uid=h.snz_uid 
left join ##srr ac
on a.snz_moe_uid=ac.snz_moe_uid and ac.year<=a.birth_year+15
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_yju_fgc] ae
on a.snz_uid=ae.snz_uid and ae.year<=a.birth_year+15
left join #siblings_alted af
on a.snz_uid=af.snz_uid and af.year<=a.birth_year+15
group by a.snz_uid

drop table if exists #pretreatment_15_3
select a.snz_uid
	-- Schools enrolled in
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_prior
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_prior
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_prior
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_prior 
	, max(case when i.rn=1 and i.decilecode != 99 then i.decilecode else null end) as first_school_decile -- 99 is used as a code in cases where schools aren't assigned a decile, so set this to null
	, max(case when i.rn=1 then i.schoolregion else null end) as first_school_region
	, sum(i.any_move) as n_schoolchanges_prior
	, sum(i.structural_move) as n_structuralchanges_prior
	, min(case when i.snz_uid is not null then 0 else 1 end) as missing_schools 
	, max(case when i.rn=1 then i.moe_esi_provider_code else null end) as providerid_firstschool
into #pretreatment_15_3
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)<=a.birth_year+15 
group by a.snz_uid

drop table if exists #pretreatment_15_4
select a.snz_uid
	-- Health: Birth outcomes
	, max(j.moh_matb_birthweight_nbr) as birth_weight
	, max(j.moh_matb_gestational_age_nbr) as birth_gestational_age
	, max(j.moh_matb_apgar_scre_aftr_5mn_nbr) as birth_apgar
	, min(case when j.snz_uid is not null then 0 else 1 end) as missing_birth 
	-- Health: Intellectual disability and neurodiversity
	, max(case when x.snz_uid is not null then 1 else 0 end) as id
	, max(case when y.snz_uid is not null then 1 else 0 end) as asd
	, max(case when z.snz_uid is not null then 1 else 0 end) as adhd
	, max(ad.child_mha_15) as child_mha
	-- Attendance at age 15
	, max(cast(k.attendance_p as float)/cast(k.attendance_total as float)*100) as attendance_15_p
	, max(cast(k.attendance_j as float)/cast(k.attendance_total as float)*100) as attendance_15_j
	, max(cast(k.attendance_u as float)/cast(k.attendance_total as float)*100) as attendance_15_u
	, min(case when k.snz_moe_uid is not null then 0 else 1 end) as missing_attendance
	-- NZDep (for first address) and number of addresses by age 15
	, max(case when l.rn=1 then l.NZDep2018 else null end) as first_nzdep
	, max(l.rn) as num_addresses_prior
	, min(case when l.snz_uid is not null then 0 else 1 end) as missing_address
	/* Info about the student's parents 
	Note here that for convenience I am categorising parent 1 as the mother and parent 2 as the father. 
	This is not strictly true for everyone, but true in the vast majority  of cases */
	-- Parental dummies
	, min(case when a.snz_parent1_uid is not null then 0 else 1 end) as missing_mother
	, min(case when a.snz_parent2_uid is not null then 0 else 1 end) as missing_father
	, max(case when a.snz_parent1_uid is not null and a.snz_parent2_uid is null then 1 else 
		case when a.snz_parent1_uid is null then null else 0 end end) as sole_parent
	, max(case when datediff(year, datefromparts(b.snz_birth_year_nbr,1,1), datefromparts(a.birth_year,1,1)) < 20 then 1 else 0 end) as teen_parent
into #pretreatment_15_4
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moh_clean].[maternity_baby] j
on a.snz_uid=j.snz_uid 
left join #attendance k
on a.snz_moe_uid=k.snz_moe_uid and k.year=a.birth_year+15
left join [IDI_Clean_202210].[data].[personal_detail] b
on a.snz_parent1_uid=b.snz_uid
left join #addresses l
on a.snz_uid=l.snz_uid and year(l.start_date)<=a.birth_year+15 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_id] x
on a.snz_uid=x.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_asd] y
on a.snz_uid=y.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_adhd] z
on a.snz_uid=z.snz_uid
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_child] ad
on a.snz_uid=ad.snz_uid
group by a.snz_uid

drop table if exists #pretreatment_15_5
select a.snz_uid
	-- Mother income/benefit
	/* Note that my current benefit definition is based on IR data, not MSD data. This is just for convenience - could change this as an extension.*/
	, max(cast(m.inc_cal_yr_sum_all_srces_tot_amt/m.cpi_2017*1000 as bigint)) as mother_totalinc_age15
	, max(cast(m.inc_cal_yr_sum_BEN_tot_amt/m.cpi_2017*1000 as bigint)) as mother_beninc_age15
	, max(case when m.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as mother_benefit_age15
	, min(case when m.snz_uid is not null then 0 else 1 end) as missing_motherinc
	-- Father income/benefit
	, max(cast(n.inc_cal_yr_sum_all_srces_tot_amt/n.cpi_2017*1000 as bigint)) as father_totalinc_age15
	, max(cast(n.inc_cal_yr_sum_BEN_tot_amt/n.cpi_2017*1000 as bigint)) as father_beninc_age15
	, max(case when n.inc_cal_yr_sum_BEN_tot_amt>0 then 1 else 0 end) as father_benefit_age15
	, min(case when n.snz_uid is not null then 0 else 1 end) as missing_fatherinc
into #pretreatment_15_5
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #parent_income m
on a.snz_parent1_uid=m.snz_uid and m.year=a.birth_year+15
left join #parent_income n
on a.snz_parent2_uid=n.snz_uid and n.year=a.birth_year+15
group by a.snz_uid

drop table if exists #pretreatment_15_6
select a.snz_uid
	-- Mother proceeded against
	, max(case when o.snz_uid is not null then 1 else 0 end) as mother_anyoffence_prior
	, max(case when left(o.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentoffence_prior
	-- Father proceeded against
	, max(case when p.snz_uid is not null then 1 else 0 end) as father_anyoffence_prior
	, max(case when left(p.[pol_poo_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentoffence_prior
	-- Mother victim of crime
	, max(case when q.snz_uid is not null then 1 else 0 end) as mother_anyvictim_prior
	, max(case when left(q.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as mother_violentvictim_prior
	-- Father victim of crime
	, max(case when r.snz_uid is not null then 1 else 0 end) as father_anyvictim_prior
	, max(case when left(r.[pol_pov_anzsoc_offence_code],2) in ('01','02','03','04','05','06') then 1 else 0 end) as father_violentvictim_prior
into #pretreatment_15_6
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] o
on a.snz_parent1_uid=o.snz_uid and o.pol_poo_year_nbr<=a.birth_year+15
left join [IDI_Clean_202210].[pol_clean].[post_count_offenders] p
on a.snz_parent2_uid=p.snz_uid and p.pol_poo_year_nbr<=a.birth_year+15
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] q
on a.snz_parent1_uid=q.snz_uid and q.pol_pov_year_nbr<=a.birth_year+15
left join [IDI_Clean_202210].[pol_clean].[post_count_victimisations] r
on a.snz_parent2_uid=r.snz_uid and r.pol_pov_year_nbr<=a.birth_year+15
group by a.snz_uid

drop table if exists #pretreatment_15_7
select a.snz_uid
	-- Mother corrections sentences
	, max(case when s.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as mother_custodial_prior
	, max(case when s.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as mother_community_prior
	-- Father corrections sentences
	, max(case when t.cor_rommp_directive_type in ('Imprisonment','Remand','Home Detention') then 1 else 0 end) as father_custodial_prior
	, max(case when t.cor_rommp_directive_type in ('Extended Supervision Order','Parole','Released on Conditions','Post Detention Conditions','Intensive Supervision'
											,'Community Detention','Supervision','Community Work','Periodic Detention','Community Programme','Community Service'
											,'Electronically Monitored Bail','Extended Supervision Order (Interim)','Returning Offender Order') then 1 else 0 end) as father_community_prior
	-- Mother highest qual
	, max(u.qual) as mother_qual
	-- Father highest qual
	, max(v.qual) as father_qual
	-- Mother mental health and addiction
	, max(aa.parent1_mh) as mother_mh
	, max(aa.parent1_aod) as mother_aod
	, max(aa.parent1_services) as mother_mhservices
	-- Father mental health and addiction
	, max(aa.parent2_mh) as father_mh
	, max(aa.parent2_aod) as father_aod
	, max(aa.parent2_services) as father_mhservices
into #pretreatment_15_7
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] s
on a.snz_parent1_uid=s.snz_uid and year(s.cor_rommp_period_start_date)<=a.birth_year+15
left join [IDI_Clean_202210].[cor_clean].[ra_ofndr_major_mgmt_period_a] t
on a.snz_parent2_uid=t.snz_uid and year(t.cor_rommp_period_start_date)<=a.birth_year+15
/* Note that for quals, I relaxed the birth year restriction somewhat (up until age 16). 
This was to use census data for more cohorts (because otherwise Census 2013 cannot be used for anyone born before 2001).
The required assumption here is that parent quals are not caused by selection into alt ed. */
left join ##parent_quals u
on a.snz_parent1_uid=u.snz_uid and u.year<=a.birth_year+16
left join ##parent_quals v
on a.snz_parent2_uid=v.snz_uid and v.year<=a.birth_year+16 
left join [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_mha_parents] aa
on a.snz_uid=aa.snz_uid
group by a.snz_uid


/* New addition 18/4/2023 - add in disability and head injury */
drop table if exists #pretreatment_15_8
select a.snz_uid
	, max(case when ag.[cen_ind_dffcl_comt_code] in (3,4) or ag.[cen_ind_dffcl_hearing_code] in (3,4) or ag.[cen_ind_dffcl_remembering_code] in (3,4) or
		ag.[cen_ind_dffcl_seeing_code] in (3,4) or ag.[cen_ind_dffcl_walking_code] in (3,4) or ag.[cen_ind_dffcl_washing_code] in (3,4) then 1 else 0 end) as disability_2018
	, max(case when ah.[cen_ind_difficulty_acty1_code]='01' or ah.[cen_ind_difficulty_acty2_code]='01' or ah.[cen_ind_difficulty_acty3_code]='01' or ah.[cen_ind_difficulty_acty4_code]='01' or
		ah.[cen_ind_difficulty_acty5_code]='01' or [cen_ind_difficulty_acty6_code]='01' or [cen_ind_difficulty_acty7_code]='01' then 1 else 0 end) as disability_2013
	, max(case when ai.snz_uid is not null then 1 else 0 end) as head_injury
into #pretreatment_15_8
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[cen_clean].[census_individual_2018] ag
on a.snz_uid=ag.snz_uid
left join [IDI_Clean_202210].[cen_clean].[census_individual_2013] ah
on a.snz_uid=ah.snz_uid
left join 
	(select *, year(case when accident_date is null then event_date else accident_date end) as year 
		from [IDI_Sandpit].[DL-MAA2021-49].CW_202210_tbi_codes 
		where code in ('S0406','Q2011','S0001','Q2002','S0116','1802','1803','S640Z','S622Z','S0301'
		,'Q2004','S0103','S0110','S64Z.','S002.','S643.','S003.','S62Z.','S644.','SYU03','S0120','SF03.','S0604','S031.','S0628','S62..','S603.','S00..'
		,'S0605','S0603','S065','S068','S0633','S061','SJ80.','S642.','S098','S0634','S610.','S000.','S0630','S011.','S069','S0638','S066','S097','S0000'
		,'S0623','S03Z.','S001.','S61..','S010.','S099','S01..','S64..','S020','S021','S064','S030.','S04..','S0620','S0622','S6460','S0601','S029','S0621'
		,'S0631','S028','S0...','S092','S0602','S602.','S091','S0600','S044.','S605.','S646.','S090')) ai -- This is a list of codes associated with heightened probability of ID
on a.snz_uid=ai.snz_uid and ai.year<=a.birth_year+15
group by a.snz_uid


/* Combine pre-treatment tables */
drop table if exists #pretreatment_15_combined
select a.*

	, b.ece_participation 
	, b.ot_report_prior
	, b.ot_invest_prior
	, b.yj_fgc_prior
	, b.ot_place_prior
	, b.mme
	, b.sibling_alted

	, c.activitycentre_prior
	, c.specialschool_prior
	, c.tekura_prior
	, c.homeschool_enrol_prior 
	, c.first_school_decile 
	, case when c.first_school_region is null then 'Unknown' else c.first_school_region end as first_school_region -- R doesn't like nulls in string variables
	, c.n_schoolchanges_prior
	, (c.n_schoolchanges_prior - c.n_structuralchanges_prior) as n_nonstructuralchanges_prior
	, c.missing_schools 
	, c.providerid_firstschool

	, d.birth_weight
	, d.birth_gestational_age
	, d.birth_apgar
	, d.missing_birth 
	, d.id
	, d.asd
	, d.adhd
	, d.child_mha
	, d.attendance_15_p
	, d.attendance_15_j
	, d.attendance_15_u
	, d.missing_attendance

	, d.first_nzdep
	, d.num_addresses_prior
	, d.missing_address
	, d.missing_mother
	, d.missing_father
	, d.teen_parent

	, e.mother_totalinc_age15
	, e.mother_beninc_age15
	, e.mother_benefit_age15
	, e.missing_motherinc
	, e.father_totalinc_age15
	, e.father_beninc_age15
	, e.father_benefit_age15
	, e.missing_fatherinc

	, f.mother_anyoffence_prior
	, f.mother_violentoffence_prior
	, f.father_anyoffence_prior
	, f.father_violentoffence_prior
	, f.mother_anyvictim_prior
	, f.mother_violentvictim_prior
	, f.father_anyvictim_prior
	, f.father_violentvictim_prior

	, g.mother_custodial_prior
	, g.mother_community_prior
	, g.father_custodial_prior
	, g.father_community_prior
	, g.mother_qual
	, g.father_qual
	, g.mother_mh
	, g.mother_aod
	, g.mother_mhservices
	, g.father_mh
	, g.father_aod
	, g.father_mhservices

	, h.disability_2018
	, h.disability_2013
	, case when h.disability_2018=1 or h.disability_2013=1 then 1 else 0 end as disability_combined
	, h.head_injury

into #pretreatment_15_combined
from #pretreatment_15_1 a
left join #pretreatment_15_2 b
on a.snz_uid=b.snz_uid
left join #pretreatment_15_3 c
on a.snz_uid=c.snz_uid
left join #pretreatment_15_4 d
on a.snz_uid=d.snz_uid
left join #pretreatment_15_5 e
on a.snz_uid=e.snz_uid
left join #pretreatment_15_6 f
on a.snz_uid=f.snz_uid
left join #pretreatment_15_7 g
on a.snz_uid=g.snz_uid
left join #pretreatment_15_8 h
on a.snz_uid=h.snz_uid


/* Create a table looking at events over ages 13-16 (target age for Alt Ed) */
drop table if exists #treatment_15_combined
select a.snz_uid
	-- Data from student intervention table (standdowns, suspensions, learning support etc)
	, max(case when d.moe_inv_intrvtn_code=6 then 1 else 0 end) as alted_during
	, min(case when d.moe_inv_intrvtn_code=6 then year(d.moe_inv_start_date) else null end) as year_alted
	, max(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_during
	, sum(case when d.moe_inv_intrvtn_code=8 then 1 else 0 end) as standdown_n_during
	, max(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_during
	, sum(case when d.moe_inv_intrvtn_code=7 then 1 else 0 end) as suspension_n_during
	, max(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_during
	, sum(case when d.moe_inv_intrvtn_code in (9,32) then 1 else 0 end) as attserv_n_during
	, max(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_during
	, sum(case when d.moe_inv_intrvtn_code=9 then 1 else 0 end) as attserv_nets_n_during
	, max(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_during
	, sum(case when d.moe_inv_intrvtn_code=32 then 1 else 0 end) as attserv_ua_n_during
	, max(case when d.moe_inv_intrvtn_code=11 then 1 else 0 end) as homeschool_during
	, max(case when d.moe_inv_intrvtn_code=38 then 1 else 0 end) as tpu_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=2 then 1 else 0 end) as behavserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=3 then 1 else 0 end) as commserv_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=5 then 1 else 0 end) as earlyint_during
	, max(case when d.moe_inv_intrvtn_code=39 and d.moe_inv_se_service_category_code=10 then 1 else 0 end) as iws_during
into #treatment_15_combined
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join [IDI_Clean_202210].[moe_clean].[student_interventions] d
on a.snz_uid=d.snz_uid and year(d.moe_inv_start_date)>=a.birth_year+16 and year(d.moe_inv_start_date)<=a.birth_year+16
group by a.snz_uid


/* Adding on a table looking at the enrolment table */
drop table if exists #treatment_15_combined_2
select a.snz_uid
	, max(case when i.providertypeid=10036 then 1 else 0 end) as activitycentre_during
	, max(case when i.providertypeid=10026 then 1 else 0 end) as specialschool_during
	, max(case when i.moe_esi_provider_code=498 then 1 else 0 end) as tekura_during
	, max(case when i.moe_esi_provider_code=972 then 1 else 0 end) as homeschool_enrol_during 
into #treatment_15_combined_2
from [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_pop] a
left join #school_changes i
on a.snz_uid=i.snz_uid and year(i.moe_esi_start_date)>=a.birth_year+16 and year(i.moe_esi_start_date)<=a.birth_year+16
group by a.snz_uid


/* Create matching table for output
This table:
	Contains all of the variables that will be used for matching
	Imputes missing data where necessary
	Applies selection criteria
	Includes whether the student was referred to alt ed between ages 13-16 (the outcome that the PSM will match on 
*/
drop table if exists #matching_15
select a.snz_uid
	-- Outcome
	, alted_during
	, year_alted
	
	-- Inclusion/exclusion criteria 
	, birth_year
	, death_year
	, pretreatment_years
	, treatment_years
	, followup_years
	, alted_prior
	, homeschool_prior
	, activitycentre_prior
	, specialschool_prior
	, tekura_prior
	, homeschool_during
	, tpu_prior
	, tpu_during
	, activitycentre_during
	, specialschool_during
	, tekura_during
	, homeschool_enrol_during 
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 then 1 else 0 end as altsetting_prior
	, case when activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_during
	, case when activitycentre_prior=1 or specialschool_prior=1 or tekura_prior=1 
		or activitycentre_during=1 or specialschool_during=1 or tekura_during=1 then 1 else 0 end as altsetting_ever

	--Demographics   
	, male
	, eth_european
	, eth_maori
	, eth_pacific
	, eth_asian
	, eth_melaa
	, eth_other
	, case when eth_maori=1 and eth_european=0 and eth_pacific=0 and eth_asian=0 and eth_melaa=0 and eth_other=0 then 1 else 0 end as sole_maori

	-- Missing variables
	, born_in_nz
	, sole_birth_parent
	, missing_address
	, missing_schools
	, missing_mother
	, missing_father
	, missing_motherinc
	, missing_fatherinc

	, ot_report_prior
	, ot_invest_prior
	, yj_fgc_prior
	, ot_place_prior
	, attserv_prior
	, ors_prior
	, esol_prior
	, behavserv_prior
	, commserv_prior
	, earlyint_prior
	, iws_prior
	, behavserv_during
	, commserv_during
	, iws_during
	, mme as mme_prior
	, sibling_alted
	, first_school_region
	, num_addresses_prior
	, teen_parent
	, id
	, asd
	, adhd
	, child_mha
	, mother_totalinc_age15
	, mother_benefit_age15
	, father_totalinc_age15
	, father_benefit_age15
	, mother_anyoffence_prior
	, mother_violentoffence_prior
	, father_anyoffence_prior
	, father_violentoffence_prior
	, mother_custodial_prior
	, mother_community_prior
	, father_custodial_prior
	, father_community_prior
	, case when mother_mh=1 or mother_aod=1 or mother_mhservices=1 then 1 else 0 end as mother_mh -- combining these three variables because they might be conceptually important but were individually non-significant
	, case when father_mh=1 or father_aod=1 or father_mhservices=1 then 1 else 0 end as father_mh 
	, case when mother_qual = 0 then '0 None' 
			when mother_qual in (1,2,3) then '1 Sch'
			when mother_qual in (4,5) then '2 Dip'
			when mother_qual in (6,7) then '3 Deg'
			when mother_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as mother_qual_cat
	, case when father_qual = 0 then '0 None' 
			when father_qual in (1,2,3) then '1 Sch'
			when father_qual in (4,5) then '2 Dip'
			when father_qual in (6,7) then '3 Deg'
			when father_qual in (8,9,10) then '4 Post'
			else '5 Unk' end as father_qual_cat

	-- Top-coding these counts so can use as binary variables
	, case when attserv_n_prior > 2 then 3 else attserv_n_prior end as attserv_n_prior
	, case when standdown_n_prior > 4 then 5 else standdown_n_prior end as standdown_n_prior
	, case when suspension_n_prior > 2 then 3 else suspension_n_prior end as suspension_n_prior
	, case when exclusion_n_prior > 2 then 3 else exclusion_n_prior end as exclusion_n_prior
	, case when n_nonstructuralchanges_prior > 2 then 3 else n_nonstructuralchanges_prior end as n_nonstructuralchanges_prior

	/* In most cases, we want to replace a missing value with a 0, along with a separate binary indicator showing the value is missing.
	However, in some cases, 0 values do not make logical sense (school deciles and NZDep). In these cases set it to 5 */
	, case when first_school_decile is null then 5 else first_school_decile end as first_school_decile
	, case when first_nzdep is null then 5 else first_nzdep end as first_nzdep

	/* Provider ID to provide entity counts */
	, a.providerid_firstschool

	/* Attendance data to check balancing */
	, attendance_15_p
	, attendance_15_j
	, attendance_15_u
	, missing_attendance

	/* New data as of 18/4/2023 */
	, disability_2018
	, disability_2013
	, disability_combined
	, head_injury

into #matching_15
from #pretreatment_15_combined a
left join #treatment_15_combined b
on a.snz_uid=b.snz_uid
left join #treatment_15_combined_2 c
on a.snz_uid=c.snz_uid


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age15_input]
select * 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age15_input]
from #matching_15
where alted_prior=0 -- didn't get referred to alted before year turned 15
	and (year_alted is null or year_alted-birth_year=16) -- didn't get referred to alted after year turned 16
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident


/***********************************************************************************************************
***************************** Robustness matching tables
***********************************************************************************************************/

/* 
This code constructs tables that support an auxillery analysis investigating evidence for endogeneity/omitted variable bias. 
(Based on ideas from Anran Zhao @ MOE.)
The logic is that we run a separate matching process without excluding the people who later get referred to AE. 
If all of our matched control group in this sample are composed of people who later receive AE, then this is evidence that AE people
are systematically different from non-AE people. On the other hand, if our control group is mostly unchanged and still composed of
"never AE" people, then our matching process is relatively robust.
*/

drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_az_robust]
select *
	, case when year_alted-birth_year > 13 then 1 else 0 end as alted_later 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age12_az_robust]
from #matching_12
where alted_prior=0 -- didn't get referred to alted before year turned 13
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_az_robust]
select * 
	, case when year_alted-birth_year > 14 then 1 else 0 end as alted_later 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age13_az_robust]
from #matching_13
where alted_prior=0 -- didn't get referred to alted before year turned 13
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident


drop table if exists [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_az_robust]
select * 
	, case when year_alted-birth_year > 15 then 1 else 0 end as alted_later 
into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_age14_az_robust]
from #matching_14
where alted_prior=0 -- didn't get referred to alted before year turned 14
	and (death_year is null or (death_year-birth_year)>16) -- didn't die before 16
	and treatment_years>0 -- have some data from 13-16 in which was a NZ resident
	and followup_years>0 -- have some data from age 17+ in which was a NZ resident


/***********************************************************************************************************
***************************** Now switch to PSM model in R
***********************************************************************************************************/

/* The R code saved in the same folder takes the matching data from the sandpit tables and constructs matching models.
The code pairs (almost) all AE students with a control learner who never enters AE. 
Separate matching models are run predicting entrance into alted at age 13,14,15,16. 
(All models are named according to the age of data used for matching - ie "_12" represents the matching model predicting AE entrance at age 13, using data up until age 12.
The subsequent matched datasets are re-saved back into the sandpit using the naming structure "[IDI_Sandpit].[DL-MAA2021-60].[ae_202210_psm_ageXX_input]"
*/



