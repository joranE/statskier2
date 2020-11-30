with ath1 as (
  select dst_result.eventid
        ,dst_result.compid
    from public.dst_result,public.dst_event
   where dst_result.eventid = dst_event.eventid
     and dst_event.date <= {max_date}
     and dst_event.date >= {min_date}
     and dst_result.eventid in (select eventid from public.dst_result where compid = {skier1} and rank is not null)
     and dst_result.eventid not in (select eventid from public.dst_result where compid = {skier2} and rank is not null)
     and dst_result.compid != {skier1}
     and dst_result.rank is not null
), ath2 as (
  select dst_result.eventid
        ,dst_result.compid
    from public.dst_result,public.dst_event
   where dst_result.eventid = dst_event.eventid
     and dst_event.date <= {max_date}
     and dst_event.date >= {min_date}
     and dst_result.eventid in (select eventid from public.dst_result where compid = {skier2} and rank is not null)
     and dst_result.eventid not in (select eventid from public.dst_result where compid = {skier1} and rank is not null)
     and dst_result.compid != {skier2}
     and dst_result.rank is not null
)

select distinct ath1.compid as com_opp_compid
from ath1,ath2
where ath1.compid = ath2.compid
