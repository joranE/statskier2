with ath as (
  select q.eventid
        ,q.compid
        ,q.nation
        ,q.rankqual
        ,q.time
        ,q.pb
        ,q.pbm
        ,q.pbm_sd
        ,q.fispoints
        ,f.spr_fin_cat
        ,f.eventid_sf
        ,f.rank
  from spr_qual_result q left outer join spr_fin_result f on q.eventid = f.eventid and q.compid = f.compid
  where q.compid = {ath_compid} or f.compid = {ath_compid}
), opp as (
  select q.eventid
        ,q.compid as opp_compid
        ,q.nation as opp_nation
        ,q.rankqual as opp_rankqual
        ,q.time as opp_time
        ,q.pb as opp_pb
        ,q.pbm as opp_pbm
        ,q.fispoints as opp_fispoints
        ,f.spr_fin_cat as opp_spr_fin_cat
        ,f.eventid_sf
        ,f.rank as opp_rank
  from spr_qual_result q left outer join spr_fin_result f on q.eventid = f.eventid and q.compid = f.compid
  where q.compid in ({opp_compid*}) or f.compid in ({opp_compid*})
)

select ev.season
      ,ev.date
      ,ev.location
      ,ev.site
      ,ev.length
      ,ev.tech
      ,t1.prim_tag as primary_tag
      ,ath.eventid
      ,ath.eventid_sf
      ,ath.spr_fin_cat
      ,ath.compid as ath_compid
      ,s1.name as ath_name
      ,ath.nation as ath_nation
      ,ath.rankqual as ath_rankqual
      ,ath.rank as ath_rank
      ,ath.time as ath_time
      ,ath.pb as ath_pb
      ,ath.pbm as ath_pbm
      ,ath.pbm_sd as ath_pmb_sd
      ,ath.fispoints as ath_fispoints
      ,opp.opp_compid
      ,s2.name as opp_name
      ,opp.opp_nation
      ,opp.opp_rankqual
      ,opp.opp_rank
      ,opp.opp_time
      ,opp.opp_pb
      ,opp.opp_pbm
      ,opp.opp_fispoints
from ath inner join opp on ath.eventid = opp.eventid and ath.eventid_sf = opp.eventid_sf
         left outer join spr_event ev on ath.eventid = ev.eventid
         left outer join skier s1 on ath.compid = s1.compid
         left outer join skier s2 on opp.opp_compid = s2.compid
         left outer join (select eventid,tag as prim_tag from event_tag where primary_tag = 'Y') t1 on ev.eventid = t1.eventid
