select
  ath.*,
  opp.opp_compid as opp_compid,
  opp.opp_fisid as opp_fisid,
  opp.opp_name as opp_name,
  opp.opp_time as opp_time,
  opp.opp_rank as opp_rank,
  opp.opp_rankqual as opp_rankqual,
  opp.opp_fispoints as opp_fispoints
from
  (
    select
      raceid,
      cat1,
      date,
      season,
      gender,
      type,
      start,
      length,
      tech,
      compid as ath_compid,
      fisid as ath_fisid,
      name as ath_name,
      time as ath_time,
      rank as ath_rank,
      rankqual as ath_rankqual,
      fispoints as ath_fispoints
    from
      main
    where
      name in @ath_name and
      type != 'Stage'
  ) ath inner join
  (
    select
      raceid,
      compid as opp_compid,
      fisid as opp_fisid,
      name as opp_name,
      time as opp_time,
      rank as opp_rank,
      rankqual as opp_rankqual,
      fispoints as opp_fispoints
    from
      main
    where
      name in @opp_name and
      type != 'Stage'
  ) opp on ath.raceid = opp.raceid
order by
  ath.raceid
