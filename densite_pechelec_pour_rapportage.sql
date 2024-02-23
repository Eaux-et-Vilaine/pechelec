/*
densite_pechelec_pour_rapportage.sql
Script de travail en vue du rapportage 2012
Travail sur les bases stacomi et EDA
Auteur Cédric Briand IAV, Virginie Berger ONEMA, Laurent Beaulaton ONEMA
Les bilans annuels sont comparés aux sorties EDA.
*/

--sauvegarde de station depuis RI_RSA vers rht
-- pg_dump -U postgres --table station -f "ri_rsa_station.sql" --verbose RI_RSA
-- restauration de la table 
-- psql -U postgres -f "ri_rsa_station.sql" eda2.0_RHT
alter table station set schema pga;
--Now running in eda2.0_RHT
--jointure spatiale
--select * from pga.station
alter table pga.station add column id_drain integer;
update pga.station set id_drain=sub1.id_drain from (
select distinct on (st_id) st_id, min(distance) as distance,id_drain from (
select st_id,r.id_drain, st_distance(st_transform(s.the_geom,3035),r.the_geom) as distance from pga.station s
join rht.rhtvs2 r on st_dwithin (st_transform(s.the_geom,3035),r.the_geom, 1000)) sub
group by st_id, distance,id_drain) sub1
where sub1.st_id=station.st_id; --173
drop table if exists pga.stationresult;
create table pga.stationresult as (select   station.st_id,
  crosstab_rhtvs2.largeur, 
  crosstab_rhtvs2.surface, 
  crosstab_rhtvs2.exutoire, 
  crosstab_rhtvs2.ers_id_drain, 
  crosstab_rhtvs2.gamma, 
  crosstab_rhtvs2.delta, 
  crosstab_rhtvs2.densite, 
  crosstab_rhtvs2.abondance, 
  crosstab_rhtvs2.nb_ar_mort_turb, 
  crosstab_rhtvs2.tx_survie_turb, 
  crosstab_rhtvs2.dmer, 
  crosstab_rhtvs2.dsource, 
  crosstab_rhtvs2.cumnbbar, 
  crosstab_rhtvs2.tx_mort_turb from pga.station  join rht.crosstab_rhtvs2  on station.id_drain=crosstab_rhtvs2.id_drain); --173

select * from pga.stationresult;
--sauvegarde de stationresult depuis rht vers RI_RSA
-- pg_dump -U postgres --table pga.stationresult -f "pga.stationresult.sql" --verbose eda2.0_RHT
-- restauration de la table 
-- psql -U postgres -c "create schema pga" RI_RSA
-- psql -U postgres -f "pga.stationresult.sql" RI_RSA

-- retour à RI_RSA

select  st_uga,op_objectifpeche,op_methodeprospection,op_moyenprospection,op_effectif_capt,densite as densite_eda from pga.stationresult s 
join operation o on o.st_id=s.st_id
join station s1 on s.st_id=s1.st_id
order by st_uga
;