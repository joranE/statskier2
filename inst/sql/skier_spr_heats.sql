select *
from public.v_sprint_heats
where public.v_sprint_heats.eventid in (
  select distinct public.spr_fin_heats.eventid
  from public.spr_fin_heats
  where public.spr_fin_heats.compid = {ath_compid}
)
