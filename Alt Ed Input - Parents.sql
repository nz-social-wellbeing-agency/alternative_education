--PURPOSE: Identify parents and caregivers from other data sources

--Rationale: if a learner has no recorded birth parents or potentially their current caregivers are 
--not their birth parents, many of these relationships will be present in other IDI data sources

--Table: Parent/Care giver Role

--Data sources:
--1. MSD (t1, t2 and t3 relationship records) primary parent and primary parent partner
--2. Working for Families (WFF) child's primary care giver and care giver's partner

--Other possible data sources - to be added in later version: 
--3. CENSUS 13 - household relationship data
--4. CENSUS 18 - household relationship data
--5. DIA Birth registrations
--6. HNZ - Social Housing data - household relationships


--Version: 1.0 19/06/2023 - WFF and MSD and DIA births added to master parent/caregiver table with spells

--NB:
--1. a child can have biological parents an caregivers that are the same - just because the caregivers are from MSD or WFF, doesn't mean they aren't the biological parents
--2. But WFF or MSD caregivers can be non-biological parents


--Notes on work still to be completed:
---------------------------------------------------
--add other data sources for parents
--join overlapping pcg spells
--Fix WFF spells before loading into the table

--Data model
--max end date based on table 2021-07-30

--POST PROCESSING TASKS
--resolve overlapping spells


--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--MSD primary care giver and child
-- end date from null to 2022/5/1


--find primary caregivers for children and spells for caregiver roles
drop table if exists #cg_child
SELECT [snz_uid]
      ,[child_snz_uid]
      ,[msd_chld_child_from_date] start_date
	  --determine end date from multiple duplicate records
      ,case when [msd_chld_child_to_date] is null then datefromparts(9999,1,1) else [msd_chld_child_to_date] end end_date
  into #cg_child
  FROM [IDI_Clean_202210].[msd_clean].[msd_child]
  --where year([msd_chld_child_to_date])=9999 

  --select * from #cg_child

  --find partners for primary caregivers and spells for partner roles
  drop table if exists #cg2_partner
  SELECT 
      [partner_snz_uid]
	  ,snz_uid
      ,msd_ptnr_ptnr_from_date start_date
	  --determine end date from multiple duplicate records
      ,case when msd_ptnr_ptnr_to_date is null then datefromparts(9999,1,1) else msd_ptnr_ptnr_to_date end end_date
into #cg2_partner
  FROM [IDI_Clean_202210].[msd_clean].[msd_partner] as a 


--find children of partner of primary caregiver and spells
  drop table if exists #cg2
  SELECT a.[snz_uid]
      ,[partner_snz_uid]
	  ,child_snz_uid
	  --determine partner start and end dates from overlaping spells
      ,case when a.start_date>=b.start_date then a.start_date else b.start_date end as start_date
      ,case when a.end_date<=b.end_date then a.end_date else b.end_date end as end_date
into #cg2
  FROM #cg2_partner as a ,#cg_child as b 
  where a.[snz_uid]=b.snz_uid and a.end_date<=b.end_date and a.start_date>=b.start_date

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--DIA births data - birth registration
--find biological parents of children
--set end date of parent spell to null

drop table if exists #bio

  select snz_uid,child_snz_uid,start_date,end_date 
  into #bio
  
  from(
  SELECT [snz_uid] as child_snz_uid
      ,datefromparts([dia_bir_birth_year_nbr],[dia_bir_birth_month_nbr],15) as start_date
	  ,datefromparts(null,null,null) as end_date
      ,[parent1_snz_uid] as snz_uid

  FROM [IDI_Clean_202210].[dia_clean].[births]
  UNION 
  
  SELECT [snz_uid] as child_snz_uid
      ,datefromparts([dia_bir_birth_year_nbr],[dia_bir_birth_month_nbr],15) as start_date
	  ,datefromparts(null,null,null) as end_date
      ,[parent2_snz_uid] as snz_uid

  FROM [IDI_Clean_202210].[dia_clean].[births]
  ) as a
  --remove where parent is unknown
  where snz_uid is not null

  --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --Working for families - primary caregiver spells and parent's partners

  --primary caregiver and children and spells
  drop table if exists #wff_cg
  SELECT [snz_uid]
      ,[child_snz_uid]
      ,[wff_chi_start_date] as start_date
	        ,case when max([wff_chi_end_date]) is null or year(max([wff_chi_end_date]))=9999 then  datefromparts(9999,1,1) else max([wff_chi_end_date]) end as end_date
	  into #wff_cg
  FROM [IDI_Clean_202210].[wff_clean].[fam_children]
  --remove children with missing snz_uids
  where child_snz_uid !=-11
  group by [snz_uid]
      ,[child_snz_uid]
      ,[wff_chi_start_date]


--partners of primary caregivers and spells
	  drop table if exists #wff_part
	  SELECT [snz_uid]
      ,[partner_snz_uid] 
      ,[wff_frp_start_date] as start_date
      ,case when max([wff_frp_end_date]) is null or year(max([wff_frp_end_date]))=9999 then  datefromparts(9999,1,1) else max([wff_frp_end_date]) end as end_date
	  into #wff_part
  FROM [IDI_Clean_202210].[wff_clean].[fam_return_parents]
  group by [snz_uid]
      ,[partner_snz_uid] 
      ,[wff_frp_start_date] 


--match partner to children of primary caregiver and spells
	  drop table if exists #wff
	  select a.partner_snz_uid as snz_uid,b.child_snz_uid
      ,case when a.start_date>=b.start_date then a.start_date else b.start_date end as start_date
      ,case when a.end_date<=b.end_date then a.end_date when b.end_date is not null then b.end_date else datefromparts(9999,1,1) end as end_date
	  ,'2' as type
	  into #wff
	  from #wff_part as a ,#wff_cg as b 
	  where a.snz_uid=b.snz_uid and a.start_date<=b.end_date and a.end_date>=b.start_date and a.partner_snz_uid !=-11


--final table of all relationships
--codes for type:
--1 : primary caregiver (uncertain if biological parent)
--2 : partner of primary caregiver (uncertain if biological parent)
--B : biological parents

  drop table if exists #all
  
  select distinct snz_uid,child_snz_uid,start_date,end_date,type
  into #all
  from 
  (
  --primary care giver
  select snz_uid,child_snz_uid,start_date,end_date ,'1' as type from #cg_child
  UNION 
  --partner
  select partner_snz_uid as snz_uid,child_snz_uid,start_date,end_date,'2' as type from #cg2
  UNION 
  --biological 
  select snz_uid,child_snz_uid,start_date,end_date,'B' as type from #bio
    UNION 
	--primary care giver
  select snz_uid,child_snz_uid,start_date,end_date,'1' as type from #wff_cg
    UNION 
	--partner
  select snz_uid,child_snz_uid,start_date,end_date,'2' as type from #wff

  ) as a
  order by child_snz_uid,snz_uid,start_date,type


--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

--output final table of parent/caregiver spells
--LOGIC: if a child has no caregiver spells for a date then they are assigned their biological parents for that date
  select * 
  into [IDI_Sandpit].[DL-MAA2021-60].[ae_202210_parents]
  from #all
  
