with ath as (
  select *
  from dst_result
  where compid = @ath_compid
), opp as (
  select eventid
        ,compid as opp_compid
        ,nation as opp_nation
        ,rank as opp_rank
        ,time as opp_time
        ,pb as opp_pb
        ,pbm as opp_pbm
        ,fispoints as opp_fispoints
  from dst_result
  where compid in @opp_compid
)

select ev.season
      ,ev.date
      ,ev.location
      ,ev.length
      ,ev.tech
      ,ev.format
      ,ev.cat1
      ,ath.eventid
      ,ath.compid as ath_compid
      ,s1.name as ath_name
      ,ath.nation as ath_nation
      ,ath.rank as ath_rank
      ,ath.time as ath_time
      ,ath.pb as ath_pb
      ,ath.pbm as ath_pbm
      ,ath.pbm_sd as ath_pmb_sd
      ,ath.fispoints as ath_fispoints
      ,opp.opp_compid
      ,s2.name as opp_name
      ,opp.opp_nation
      ,opp.opp_rank
      ,opp.opp_time
      ,opp.opp_pb
      ,opp.opp_pbm
      ,opp.opp_fispoints
from ath inner join opp on ath.eventid = opp.eventid
         left outer join dst_event ev on ath.eventid = ev.eventid
         left outer join skier s1 on ath.compid = s1.compid
         left outer join skier s2 on opp.opp_compid = s2.compid
