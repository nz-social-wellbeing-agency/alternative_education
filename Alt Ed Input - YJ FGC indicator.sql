

--PURPOSE: create indicators of whther children have had CNP and YJU interventions of :
--APPLICATION: SWA Alternative Education outcome PSM evaluation
--Project and sandpit: idi_sandpit.[DL-MAA2021-60]

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
--final table only includes learners in the ae study population
--AE evaluation learners reference table: 

select top 10 * from IDI_Sandpit.[DL-MAA2021-60].ae_202210_pop

--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

--Description: ALternative Education is a compulsary education intervention for alienated students

---1. FCG : family group conversence
--2. FWA : family whanau agreement

--NB: subset for Alt Ed cohorts only

--Author: Craig Wright
--date: 07/3/2023
--Version: 1.0 

--Refesh note: IDI_refresh_202210:  seems complete from 2007 to 2021 and 6 months of 2022


--Summary of final indicator table :

--Variables:
--SNZ_UID : IDI person UID for refresh 202210
--year : year of fgc or fwa
--type : whether YOuth JUstice YJU or Care and protection (OT) CNP
--ot_intervention : investigation outcome type = FGC or FWA
--FGC	Family Group Conference
--FWA	Family Whanau Agreement


--EVENTS : number of internvetions in calendar year

  drop table if exists IDI_Sandpit.[DL-MAA2021-60].ae_202210_ot_interventions 

  --create aggregate summary table in project sandpit

    select snz_uid,year,type,ot_intervention,sum(events) as events
  into IDI_Sandpit.[DL-MAA2021-60].ae_202210_ot_interventions 
  from (

  --read fgc events
  SELECT a.[snz_uid]
      --,[snz_msd_uid]
      --,a.[snz_composite_event_uid]
      --,[cyf_fge_event_from_date_wid_date]
	  ,year([cyf_fge_event_from_date_wid_date]) as year
      --,[cyf_fge_event_to_date_wid_date]
	  ,b.cyf_fgd_business_area_type_code as type
	  ,'FGC' ot_intervention
	  ,1 as events
  --inner join to retain only AE evaluation cohorts
  FROM [IDI_Clean_202210].[cyf_clean].[cyf_ev_cli_fgc_cys_f] as a ,[IDI_Clean_202210].[cyf_clean].[cyf_dt_cli_fgc_cys_d] as b,IDI_Sandpit.[DL-MAA2021-60].ae_202210_pop as c
  where a.snz_composite_event_uid=b.snz_composite_event_uid and a.snz_uid=c.snz_uid
  UNION 

  --read wfa events
  SELECT a.[snz_uid]
      --,[snz_msd_uid]
      --,a.[snz_composite_event_uid]
      --,[cyf_fge_event_from_date_wid_date]
	  ,year(a.cyf_fwe_event_from_date_wid_date ) as year
      --,[cyf_fge_event_to_date_wid_date]
	  ,b.cyf_fwd_business_area_type_code as type
	  ,'WFA' ot_intervention
	  ,1 as events
  --inner join to retain only AE evaluation cohorts
  FROM [IDI_Clean_202210].[cyf_clean].[cyf_ev_cli_fwas_cys_f] as a ,[IDI_Clean_202210].[cyf_clean].[cyf_dt_cli_fwas_cys_d] as b,IDI_Sandpit.[DL-MAA2021-60].ae_202210_pop as c
  where a.snz_composite_event_uid=b.snz_composite_event_uid and a.snz_uid=c.snz_uid
  

) as a

  group by snz_uid,year,type,ot_intervention



  --review final indicator table
select top 100 * from IDI_Sandpit.[DL-MAA2021-60].ae_202210_ot_interventions 
order by snz_uid,year

--summarise final table
select type,ot_intervention,year,sum(events) as events,count(distinct snz_uid) as ppl 
from (
select * from IDI_Sandpit.[DL-MAA2021-60].ae_202210_ot_interventions 
) as a
  group by type,ot_intervention,year
  order by type,ot_intervention,year