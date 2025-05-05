
# connect to wordpress-DB ----
wp_conn <- get_wp_conn()

if (typeof(wp_conn) != "S4") {
  stop()
}

# artists with at least 4 broadcasts ----
qry <- "
with ds1 as (
select t2.name as artist_name,
       p1.post_title as bc_name,
       p1.post_date as bc_date,
       p1.ID as bc_id,
       p1.post_type as bc_type,
       t2.term_id as artist_id,
       ROW_NUMBER() OVER (PARTITION BY t2.name ORDER BY p1.post_title) AS row_num
from wp_posts p1
   join wp_term_relationships r1 on r1.object_id = p1.ID
   join wp_term_taxonomy t1 on t1.term_taxonomy_id = r1.term_taxonomy_id
   join wp_terms t2 on t2.term_id = t1.term_id
where t1.term_taxonomy_id in (select term_taxonomy_id from wp_term_taxonomy where taxonomy = 'programma_componist')
  and p1.post_status = 'publish'
  and p1.ID in (select object_id from wp_term_relationships where term_taxonomy_id = 5) -- NL-posts only
)
select artist_name, artist_id from ds1 where row_num = 4;
"
qry_rst <- dbGetQuery(wp_conn, qry)

# only new ones, so remove artist-id's already processed in any sts_300 tibble
ls_wd_artists_300 <- dir_ls(path = "h:/artist_resolver/sts_300/", type = "file", regexp = "\\.RDS$")

if (length(ls_wd_artists_300) > 0) {
  combined_data_300 <- map_dfr(ls_wd_artists_300, ~ read_rds(.x)) |> rename(artist_id = artist_czid)
  qry_rst <- qry_rst |> filter(!artist_id %in% combined_data_300$artist_id)
}

qfn <- "h:/artist_resolver/sts_100/cz_artists_4p.RDS"
if (nrow(qry_rst) > 0) {
  write_rds(qry_rst, qfn)
} else {
  file_delete(qfn)
  cat("no new artists found\n")
}

# valid artists ----
qry <- "
with ds1 as (
   select * from wp_terms tm1 join wp_term_relationships tr1 on tr1.object_id = tm1.term_id
), ds2 as (
   select tm2.term_id from wp_term_taxonomy tx1
					  JOIN wp_terms tm2
                        ON tm2.term_id = tx1.term_taxonomy_id
   WHERE tx1.taxonomy = 'programma_componist'
), ds3 as (
   select ds1.* from ds1
                join ds2
                  on ds2.term_id = ds1.term_id
), ds3a as (
   select name, count(*) as n from ds3
   where term_taxonomy_id in (89, 90)
   group by name
), ds4 as (
   select ds3.*, n from ds3
                   join ds3a
					 on ds3a.name = ds3.name
   where term_taxonomy_id = 89
), ds5 as (
   select ds3.*, n from ds3
                   join ds3a
                     on ds3a.name = ds3.name
   where term_taxonomy_id = 90
)
select ds5.object_id as artist_czid_NL,
       ds4.object_id as artist_czid_EN,
       ds5.name as artist_name
from ds5 left join ds4
				on ds4.name = ds5.name
where ds4.n = 2
order by ds5.object_id;
"
qry_rst <- dbGetQuery(wp_conn, qry)
write_rds(qry_rst, "h:/artist_resolver/sts_100/cz_artists_valid.RDS")

# invalid artists ----
qry <- "
with ds1 as (
   select * from wp_terms tm1 join wp_term_relationships tr1 on tr1.object_id = tm1.term_id
), ds2 as (
   select tm2.term_id from wp_term_taxonomy tx1
					  JOIN wp_terms tm2
                        ON tm2.term_id = tx1.term_taxonomy_id
   WHERE tx1.taxonomy = 'programma_componist'
), ds3 as (
   select ds1.* from ds1
                join ds2
                  on ds2.term_id = ds1.term_id
), ds3a as (
   select name, count(*) as n from ds3
   where term_taxonomy_id in (89, 90)
   group by name
), ds4 as (
   select ds3.*, n from ds3
                   join ds3a
					 on ds3a.name = ds3.name
   where term_taxonomy_id = 89
), ds5 as (
   select ds3.*, n from ds3
                   join ds3a
                     on ds3a.name = ds3.name
   where term_taxonomy_id = 90
)
select ds5.name as name_NL,
       ds5.slug as slug_NL,
       ds5.n as n,
       ds5.object_id as id_NL,
       ds4.name as name_EN,
       ds4.slug as slug_EN,
       ds4.object_id as id_EN
from ds5 left join ds4 on ds4.name = ds5.name
where ds4.n != 2
order by ds5.name;
"
qry_rst <- dbGetQuery(wp_conn, qry)
write_tsv(qry_rst, "h:/artist_resolver/sts_100/cz_artists_invalid.tsv")

sql_rst <- dbDisconnect(wp_conn)
