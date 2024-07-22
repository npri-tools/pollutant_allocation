/* COMPILE REPORTS */
/* Methods of estimating substance quantities*/
DROP VIEW IF EXISTS detail_npri_substanceestimates CASCADE;
CREATE VIEW detail_npri_substanceestimates AS
	SELECT "SubstanceQuantityID", "SubstanceReportID", "CategoryTypeID", "DescriptionEn" as "Method", "Quantity" FROM NPRI_SUBSTANCE_QUANTITY 
	LEFT JOIN DETAIL_NPRI_BASISOFESTIMATE ON DETAIL_NPRI_BASISOFESTIMATE."BasisOfEstimateTypeID" = NPRI_SUBSTANCE_QUANTITY."BasisOfEstimateTypeID";

/* Media substances reported being released to */
DROP VIEW IF EXISTS detail_npri_substancemedia CASCADE;
CREATE VIEW detail_npri_substancemedia AS
	SELECT "SubstanceQuantityID", "SubstanceReportID", DETAIL_NPRI_CATEGORYTYPE."CategoryTypeID", "Method", "Quantity", "DescriptionEn", "GroupEn" FROM detail_npri_substanceestimates
	LEFT JOIN DETAIL_NPRI_CATEGORYTYPE ON DETAIL_NPRI_CATEGORYTYPE."CategoryTypeID" = detail_npri_substanceestimates."CategoryTypeID";

/* Reports with Substances Information */
/* TBD: is there a better way than dropping "IsDeleted"? Knowing deleted reports is important; can surface facilities not previously known */
DROP VIEW IF EXISTS detail_npri_substances CASCADE;
CREATE VIEW detail_npri_substances AS
	SELECT "SubstanceQuantityID", selection."SubstanceReportID", "Quantity", "Method", "DescriptionEn", "GroupEn", selection."SubstanceID", "Threshold", "ReportID",
		"Cas", "IsVoc", "IsDf", "IsPAH"
		FROM detail_npri_substancemedia
			RIGHT JOIN (SELECT * FROM NPRI_REPORT_SUBSTANCE WHERE NPRI_REPORT_SUBSTANCE."IsDeleted" = False) as selection ON selection."SubstanceReportID" = detail_npri_substancemedia."SubstanceReportID"   /* test right join here for vocs and other issues */
			LEFT JOIN DETAIL_NPRI_SUBSTANCE USING ("SubstanceID");

/* Substances Units */
/* These seem to be year-specific, so aren't joined above in detail_npri_substances - they're only joined with the reports, which have ReportYear */
DROP VIEW IF EXISTS detail_npri_substancesunits CASCADE;
CREATE VIEW detail_npri_substancesunits AS
	SELECT "SubstanceInfoID", "SubstanceID", "ReportYear", "NameEn", "UnitTypeID", "NPRI", "NPRI_Part", "Units" FROM DETAIL_NPRI_SUBSTANCEINFO
	LEFT JOIN (SELECT "NameEn" as "Units", "UnitTypeID" as "UTID" FROM DETAIL_UNITTYPE) AS UNITS ON UNITS."UTID" = DETAIL_NPRI_SUBSTANCEINFO."UnitTypeID";

/* Create a de-duplicated geographiclocation table */
/* TBD: Investigate why there are multiple geo locations for some Npri Ids */
CREATE INDEX IF NOT EXISTS "NpriId" ON geographiclocation USING gist ("geom");
CREATE INDEX IF NOT EXISTS "dauid" ON lda_000b21a_e USING gist ("geom");

DROP TABLE IF EXISTS geographiclocation_unique CASCADE;
CREATE TABLE geographiclocation_unique AS
	SELECT DISTINCT ON ("NpriId") *
	FROM geographiclocation;
CREATE INDEX "NpriIdGeo" ON geographiclocation_unique USING gist ("geom");

/* Get dauids for each facility - what da is each facility contained by?*/
DROP VIEW IF EXISTS facility_da CASCADE;
CREATE VIEW facility_da AS
	SELECT geographiclocation_unique."NpriId" as "NpriID", lda_000b21a_e."dauid" 
	FROM geographiclocation_unique, lda_000b21a_e
	WHERE St_intersects(geographiclocation_unique.geom,lda_000b21a_e.geom);
/* Add a "DA" column to the npri_report table to assist in later analyses */
ALTER TABLE NPRI_REPORT ADD COLUMN IF NOT EXISTS "DA" int;
UPDATE NPRI_REPORT SET "DA" = facility_da."dauid" FROM (SELECT * FROM facility_da) as facility_da WHERE NPRI_REPORT."NpriID" = facility_da."NpriID";

/* Reports with Substances Information and other Context - NPRI IDs, DAs, NAICS, etc.*/
/* Do not include null substance report IDs - these were likely reports on other matters, not releases ??? */
DROP MATERIALIZED VIEW IF EXISTS detail_npri_reports CASCADE;
CREATE MATERIALIZED VIEW detail_npri_reports AS
	SELECT NPRI_REPORT."ReportID", NPRI_REPORT."ReportYear", "ReportTypeID", "Is_Deleted_Indicator",
		"SubstanceQuantityID", "SubstanceReportID", subs."SubstanceID", 
		"NameEn" as "Substance", "Quantity", "Units", "GroupEn" as "MediaGroup", "DescriptionEn" as "MediaMethod", "Method" as "MeasureMethod", 
		"Threshold", "NPRI" as "NPRI_tracked", "NPRI_Part", "Cas", "IsVoc", "IsDf", "IsPAH",
		"NpriID", "CompanyID", "FacilityID", "FacilityName", "DA",
		"NAICSPrimary", "NAICSTitleEn" FROM /* guessing at the important columns */
		(SELECT * FROM detail_npri_substances WHERE "SubstanceReportID" IS NOT NULL) as subs
			LEFT JOIN NPRI_REPORT ON NPRI_REPORT."ReportID" = subs."ReportID"
			LEFT JOIN detail_npri_substancesunits ON detail_npri_substancesunits."SubstanceID" = subs."SubstanceID" AND detail_npri_substancesunits."ReportYear" = NPRI_REPORT."ReportYear"
			LEFT JOIN (select "FacilityId" as "FacilityID", "NAICSPrimary", "FacilityName", "NAICSTitleEn" FROM facility as f LEFT JOIN detail_naics AS d ON d."NAICSCode" = f."NAICSPrimary" WHERE d."RecordYear" = 2022) AS j USING ("FacilityID"); 	

/* TBD: PROCESS RECORDS WITH PANDAS
Basically, do the following up to the materialized views, but using Pandas due to its flexibility (e.g. unstack)
*/

/* DERIVATIVE VIEWS AND TABLES */
/* create a measure of the different ways emissions are reported (modeled, direct, etc.) 
a measure that could be stored as a separate column field in the Facilities table (i.e. percent of reports that were modeled).
*/

/* Active Years */
DROP VIEW IF EXISTS active_years;
CREATE VIEW active_years AS
	SELECT 
		"NpriID",
		string_agg(DISTINCT "ReportYear"::text, ', ') as "ActiveYears",
		COUNT (DISTINCT "ReportYear"::text) as "NumberOfActiveYears"
	FROM
		detail_npri_reports
	GROUP BY
		"NpriID"
	ORDER BY
		"NpriID";

/* Summarize quantities releases for each id, each year */
DROP VIEW IF EXISTS quantities_by_id_year;
CREATE VIEW quantities_by_id_year AS
	SELECT
		"NpriID", "ReportYear", "Substance", "MediaGroup", "MediaMethod", "MeasureMethod",
		SUM (CASE WHEN "Units" = 'kg' THEN "Quantity"/1000 WHEN "Units" = 'grams' THEN "Quantity"/1000000 WHEN "Units" = 'g_teq_t' THEN "Quantity"/1000000 ELSE "Quantity" END) as "SumInTonnes"
	FROM
		detail_npri_reports
	GROUP BY
		"NpriID", "ReportYear", "Substance", "MediaGroup", "MediaMethod", "MeasureMethod"
	ORDER BY
		"NpriID", "ReportYear", "Substance";

/* Summarize kinds of releases for each id, each year */
DROP VIEW IF EXISTS types_by_id_year;
CREATE VIEW types_by_id_year AS
	SELECT 
		"NpriID",
		"ReportYear",
		COUNT (DISTINCT "ReportID") as "NumberOfReports",
		COUNT (DISTINCT "Substance") as "NumberOfSubstances",
		COUNT (DISTINCT "MediaGroup") as "NumberOfMedia",
		COUNT (DISTINCT "MeasureMethod") as "NumberOfMeasureMethods",
		COUNT (DISTINCT CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs",
		COUNT (DISTINCT CASE WHEN "IsPAH" THEN "Substance" END) as "NumberOfPAHs",
		string_agg(DISTINCT "Substance", ', ') as "Substances",
		string_agg(DISTINCT "Cas", ', ') as "CAS"
	FROM
		detail_npri_reports
	GROUP BY
		"NpriID", "ReportYear"
	ORDER BY
		"NpriID", "ReportYear";
   
/* Criteria Air Contaminants (CAC) substance amounts by year */
DROP VIEW IF EXISTS cac_by_id_year;
CREATE VIEW cac_by_id_year AS
	select "NpriID", "ReportYear", "Substance",
		SUM (CASE WHEN "Units" = 'kg' THEN "Quantity"/1000 WHEN "Units" = 'grams' THEN "Quantity"/1000000 WHEN "Units" = 'g_teq_t' THEN "Quantity"/1000000 ELSE "Quantity" END) as "SumInTonnes"
		from detail_npri_reports where "Substance" in ('PM10 - Particulate Matter <= 10 Micrometers', 'PM2.5 - Particulate Matter <= 2.5 Micrometers', 
		'Sulphur dioxide', 'Carbon monoxide', 'Nitrogen oxides (expressed as nitrogen dioxide)','Volatile Organic Compounds (Total)', 'Ammonia (total)') 
		and "MediaGroup" = 'Releases to Air' group by "NpriID", "ReportYear", "Substance" order by "NpriID";
		
/* Criteria Air Contaminants (CAC) substance amounts as columns - FOR A SPECIFIC YEAR, transposed*/
/* https://stackoverflow.com/questions/3002499/postgresql-crosstab-query */
CREATE EXTENSION IF NOT EXISTS tablefunc;
DROP VIEW IF EXISTS cac_by_id_current CASCADE;
CREATE VIEW cac_by_id_current AS
	SELECT *
	FROM   crosstab(
	   'SELECT "NpriID", "Substance", "SumInTonnes"
		FROM    cac_by_id_year
		WHERE   "ReportYear" = 2022 
		ORDER  BY 1,2'
	  , $$VALUES ('Carbon monoxide'), ('Sulphur dioxide'), ('Ammonia (total)'), ('PM10 - Particulate Matter <= 10 Micrometers'), ('PM2.5 - Particulate Matter <= 2.5 Micrometers'), ('Nitrogen oxides (expressed as nitrogen dioxide)'),('Volatile Organic Compounds (Total)') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "Carbon monoxide" float, "Sulphur dioxide" float, "Ammonia (total)" float, "PM10 - Particulate Matter <= 10 Micrometers" float,
					  "PM2.5 - Particulate Matter <= 2.5 Micrometers" float, "Nitrogen oxides (expressed as nitrogen dioxide)" float, "Volatile Organic Compounds (Total)" float
					  );
					  
/* Criteria Air Contaminants (CAC) substance amounts as columns - FOR A RANGE (PAST 5 YEARS), transposed*/
/* https://stackoverflow.com/questions/3002499/postgresql-crosstab-query */
DROP VIEW IF EXISTS cac_by_id_fiveyears;
CREATE VIEW cac_by_id_fiveyears AS
	SELECT *
	FROM   crosstab(
	   'SELECT * FROM (SELECT "NpriID", "Substance", SUM("SumInTonnes") as "SumInTonnes"
		FROM cac_by_id_year 
		WHERE   "ReportYear" <= 2022 AND "ReportYear" >= 2018
		GROUP BY ROLLUP("NpriID", "Substance")
		ORDER BY 1,2) as r where "Substance" is not Null; /* Null values are grand totals */'
	  , $$VALUES ('Carbon monoxide'), ('Sulphur dioxide'), ('Ammonia (total)'), ('PM10 - Particulate Matter <= 10 Micrometers'), ('PM2.5 - Particulate Matter <= 2.5 Micrometers'), ('Nitrogen oxides (expressed as nitrogen dioxide)'),('Volatile Organic Compounds (Total)') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "Carbon monoxide" float, "Sulphur dioxide" float, "Ammonia (total)" float, "PM10 - Particulate Matter <= 10 Micrometers" float,
					  "PM2.5 - Particulate Matter <= 2.5 Micrometers" float, "Nitrogen oxides (expressed as nitrogen dioxide)" float, "Volatile Organic Compounds (Total)" float
					  );					  

/* Criteria Air Contaminants (CAC) substance amounts as columns - FOR A RANGE (PAST 15 YEARS), transposed*/
/* https://stackoverflow.com/questions/3002499/postgresql-crosstab-query */
DROP VIEW IF EXISTS cac_by_id_fifteenyears;
CREATE VIEW cac_by_id_fifteenyears AS
	SELECT *
	FROM   crosstab(
	   'SELECT * FROM (SELECT "NpriID", "Substance", SUM("SumInTonnes") as "SumInTonnes"
		FROM cac_by_id_year 
		WHERE   "ReportYear" <= 2022 AND "ReportYear" >= 2008
		GROUP BY ROLLUP("NpriID", "Substance")
		ORDER BY 1,2) as r where "Substance" is not Null; /* Null values are grand totals */'
	  , $$VALUES ('Carbon monoxide'), ('Sulphur dioxide'), ('Ammonia (total)'), ('PM10 - Particulate Matter <= 10 Micrometers'), ('PM2.5 - Particulate Matter <= 2.5 Micrometers'), ('Nitrogen oxides (expressed as nitrogen dioxide)'),('Volatile Organic Compounds (Total)') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "Carbon monoxide" float, "Sulphur dioxide" float, "Ammonia (total)" float, "PM10 - Particulate Matter <= 10 Micrometers" float,
					  "PM2.5 - Particulate Matter <= 2.5 Micrometers" float, "Nitrogen oxides (expressed as nitrogen dioxide)" float, "Volatile Organic Compounds (Total)" float
					  );					  

/* Criteria Air Contaminants (CAC) substance amounts as columns - TOTAL, transposed*/
/* https://stackoverflow.com/questions/3002499/postgresql-crosstab-query */
DROP VIEW IF EXISTS cac_by_id_total;
CREATE VIEW cac_by_id_total AS
	SELECT *
	FROM   crosstab(
	   'SELECT * FROM (SELECT "NpriID", "Substance", SUM("SumInTonnes") as "SumInTonnes"
		FROM cac_by_id_year 
		GROUP BY ROLLUP("NpriID", "Substance")
		ORDER BY 1,2) as r where "Substance" is not Null; /* Null values are grand totals */'
	  , $$VALUES ('Carbon monoxide'), ('Sulphur dioxide'), ('Ammonia (total)'), ('PM10 - Particulate Matter <= 10 Micrometers'), ('PM2.5 - Particulate Matter <= 2.5 Micrometers'), ('Nitrogen oxides (expressed as nitrogen dioxide)'),('Volatile Organic Compounds (Total)') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "Carbon monoxide" float, "Sulphur dioxide" float, "Ammonia (total)" float, "PM10 - Particulate Matter <= 10 Micrometers" float,
					  "PM2.5 - Particulate Matter <= 2.5 Micrometers" float, "Nitrogen oxides (expressed as nitrogen dioxide)" float, "Volatile Organic Compounds (Total)" float
					  );					  
					  
/* Rename columns for joining later in the materialized view */
ALTER VIEW cac_by_id_current RENAME COLUMN "Carbon monoxide" to "Carbon monoxide - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "Sulphur dioxide" to "Sulphur dioxide - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "Ammonia (total)" to "Ammonia (total) - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "PM10 - Particulate Matter <= 10 Micrometers" to "PM10 - Particulate Matter <= 10 Micrometers - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "PM2.5 - Particulate Matter <= 2.5 Micrometers" to "PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "Nitrogen oxides (expressed as nitrogen dioxide)" to "Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent";
ALTER VIEW cac_by_id_current RENAME COLUMN "Volatile Organic Compounds (Total)" to "Volatile Organic Compounds (Total) - Most Recent";

ALTER VIEW cac_by_id_total RENAME COLUMN "Carbon monoxide" to "Carbon monoxide - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "Sulphur dioxide" to "Sulphur dioxide - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "Ammonia (total)" to "Ammonia (total) - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "PM10 - Particulate Matter <= 10 Micrometers" to "PM10 - Particulate Matter <= 10 Micrometers - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "PM2.5 - Particulate Matter <= 2.5 Micrometers" to "PM2.5 - Particulate Matter <= 2.5 Micrometers - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "Nitrogen oxides (expressed as nitrogen dioxide)" to "Nitrogen oxides (expressed as nitrogen dioxide) - All Years";
ALTER VIEW cac_by_id_total RENAME COLUMN "Volatile Organic Compounds (Total)" to "Volatile Organic Compounds (Total) - All Years";

ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "Carbon monoxide" to "Carbon monoxide - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "Sulphur dioxide" to "Sulphur dioxide - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "Ammonia (total)" to "Ammonia (total) - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "PM10 - Particulate Matter <= 10 Micrometers" to "PM10 - Particulate Matter <= 10 Micrometers - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "PM2.5 - Particulate Matter <= 2.5 Micrometers" to "PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "Nitrogen oxides (expressed as nitrogen dioxide)" to "Nitrogen oxides (expressed as nitrogen dioxide) - Past 5 Years";
ALTER VIEW cac_by_id_fiveyears RENAME COLUMN "Volatile Organic Compounds (Total)" to "Volatile Organic Compounds (Total) - Past 5 Years";

ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "Carbon monoxide" to "Carbon monoxide - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "Sulphur dioxide" to "Sulphur dioxide - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "Ammonia (total)" to "Ammonia (total) - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "PM10 - Particulate Matter <= 10 Micrometers" to "PM10 - Particulate Matter <= 10 Micrometers - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "PM2.5 - Particulate Matter <= 2.5 Micrometers" to "PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "Nitrogen oxides (expressed as nitrogen dioxide)" to "Nitrogen oxides (expressed as nitrogen dioxide) - Past 15 Years";
ALTER VIEW cac_by_id_fifteenyears RENAME COLUMN "Volatile Organic Compounds (Total)" to "Volatile Organic Compounds (Total) - Past 15 Years";

/*TBD: counts of CACs released */

/* Releases by MediaGroups */
DROP VIEW IF EXISTS media_by_id_year;
CREATE VIEW media_by_id_year AS
	select "NpriID", "ReportYear", "MediaGroup", 
		SUM (CASE WHEN "Units" = 'kg' THEN "Quantity"/1000 WHEN "Units" = 'grams' THEN "Quantity"/1000000 WHEN "Units" = 'g_teq_t' THEN "Quantity"/1000000 ELSE "Quantity" END) as "SumInTonnes"
		from detail_npri_reports where "MediaGroup" not like '%Sum%' 
		group by "NpriID", "MediaGroup", "ReportYear" order by "NpriID";

/*Air flags */
DROP VIEW IF EXISTS air_flags;
CREATE VIEW air_flags AS
	select distinct "NpriID", 'Y' as "AIR_FLAG" from media_by_id_year where "MediaGroup" = 'Releases to Air';		

/* Releases by MediaGroups for a specific year, transposed 
TBD: previous years
*/
DROP VIEW IF EXISTS media_by_id_current;
CREATE VIEW media_by_id_current AS
	SELECT *
	FROM   crosstab(
	   'SELECT "NpriID", "MediaGroup", "SumInTonnes"
		FROM    media_by_id_year
		WHERE   "ReportYear" = 2022
		ORDER  BY 1,2'
	  , $$VALUES ('On-site Disposal'), ('Off-site Recycling'), ('Releases to Air'), ('Off-site Transfers for Recycling'), 
		('Off-site Transfers for Treatment Prior to Final Disposal'), ('Releases to Land)'),('Off-site Disposal'),('Releases to Water Bodies') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "On-site Disposal" float, "Off-site Recycling" float, "Releases to Air" float, "Off-site Transfers for Recycling" float,
					  "Off-site Transfers for Treatment Prior to Final Disposal" float, "Releases to Land" float, "Off-site Disposal" float, "Releases to Water Bodies" float
					  );

/* Total Fugitive Emissions 
TBD: previous years
*/
DROP VIEW IF EXISTS fugitive_by_id_year;
CREATE VIEW fugitive_by_id_year AS
	select "NpriID", "ReportYear", "MediaMethod", 
		SUM (CASE WHEN "Units" = 'kg' THEN "Quantity"/1000 WHEN "Units" = 'grams' THEN "Quantity"/1000000 WHEN "Units" = 'g_teq_t' THEN "Quantity"/1000000 ELSE "Quantity" END) as "SumInTonnes"
		from detail_npri_reports where "MediaMethod" like '%Fugitive%' 
		group by "NpriID", "ReportYear", "MediaMethod" order by "NpriID";

/* Fugitive Releases for a specific year, transposed */
DROP VIEW IF EXISTS fugitive_by_id_current;
CREATE VIEW fugitive_by_id_current AS
	SELECT *
	FROM   crosstab(
	   'SELECT "NpriID", "MediaMethod", "SumInTonnes"
		FROM    fugitive_by_id_year
		WHERE   "ReportYear" = 2022
		ORDER  BY 1,2'
	  , $$VALUES ('Fugitive') $$
	   ) AS "SumInTonnes" ("NpriID" integer, "Fugitive" float);

/* GEOGRAPHY */
/* Buffers */
DROP TABLE IF EXISTS buffer_1km, buffer_3km, buffer_5km CASCADE;
CREATE TABLE buffer_1km AS SELECT ST_Buffer(geom,1000), "NpriId" as "NpriID" FROM geographiclocation_unique;
CREATE TABLE buffer_3km AS SELECT ST_Buffer(geom,3000), "NpriId" as "NpriID" FROM geographiclocation_unique;
CREATE TABLE buffer_5km AS SELECT ST_Buffer(geom,5000), "NpriId" as "NpriID" FROM geographiclocation_unique;

/* Which DAs does each facility buffer _contain_? */
DROP VIEW IF EXISTS DisseminationAreas1km CASCADE;
CREATE VIEW DisseminationAreas1km AS
	SELECT "NpriID", string_agg(DISTINCT selection."dauid"::text, ', ') as "DisseminationAreas1km"
	FROM (
		SELECT das.dauid, b."NpriID" /* ::int */
		FROM lda_000b21a_e AS das, buffer_1km as b
		WHERE ST_Within(das.geom, b.st_buffer)
		) as selection
	GROUP BY "NpriID"
	ORDER BY "NpriID";

/* how many other facilities within 1, 3, 5km?*/
DROP VIEW IF EXISTS facilities_nearby_1km CASCADE;
CREATE VIEW facilities_nearby_1km AS
	SELECT "NpriID", count(DISTINCT selection."NpriId") -1 as "OtherFacilitiesNearby1km" /* minus one to not include self */
	FROM (
		SELECT b."NpriID",facs."NpriId" /* ::int */
		FROM geographiclocation_unique AS facs, buffer_1km as b
		WHERE ST_Within(facs.geom, b.st_buffer) /* AND (b."NpriID" = 15 OR b."NpriID" = 109) */
		) as selection
	GROUP BY "NpriID"
	ORDER BY "NpriID";
	
/* Need to figure out a way to automatically handle adding DA IDs to the NPRI_REPORT table so that they can be joined to detail_npri_reports for later aggregation 
CREATE OR REPLACE TRIGGER UpdateDA AFTER UPDATE ON facility_da
DO INSTEAD
UPDATE NPRI_REPORT as n
SET "DA" = "dauid"
WHERE n."NpriID" = "NpriID";
*/

/* CONTEXT DATA */
/* Summary Census info for each NPRI ID by buffer */
/* About 3000 facilities contain DAs in 1km buffer - for the others, we could do "within" */
/* Change column name for consistency */
ALTER TABLE IF EXISTS cimd_2016
	RENAME COLUMN "PRCDDA" TO "dauid";
DROP VIEW IF EXISTS buffer_1km_cimd2016 CASCADE;
CREATE VIEW buffer_1km_cimd2016 AS
	SELECT "NpriID", 
		percentile_cont(0.50) within group (order by "Residential instability Scores") as median_instability_2016,
		percentile_cont(0.50) within group (order by "Economic dependency Scores") as median_dependency_2016,
		percentile_cont(0.50) within group (order by "Ethno-cultural composition Scores") as median_composition_2016,
		percentile_cont(0.50) within group (order by "Situational vulnerability Scores") as median_vulnerability_2016
		FROM (
			SELECT * FROM (SELECT b."NpriID", das.dauid FROM lda_000b21a_e AS das, buffer_1km as b WHERE ST_Within(das.geom, b.st_buffer)) as selection  /* ::int */
			LEFT JOIN cimd_2016 USING ("dauid")
		) as summary
	GROUP BY "NpriID"
	ORDER BY "NpriID";
DROP VIEW IF EXISTS facility_1km_cimd2016;
CREATE VIEW facility_1km_cimd2016 AS
	SELECT * FROM (SELECT "NpriID" FROM facility_da WHERE "NpriID" in (SELECT "NpriID" FROM active_years WHERE "ActiveYears" LIKE '%201%')) as selection /* NOTE: filter to 2010s for CIMD 2016 */
	LEFT JOIN buffer_1km_cimd2016 USING ("NpriID")
	ORDER BY "NpriID";

ALTER TABLE IF EXISTS cimd_2021
	RENAME COLUMN "PRCDDA" TO "dauid";
DROP VIEW IF EXISTS buffer_1km_cimd2021 CASCADE;
CREATE VIEW buffer_1km_cimd2021 AS
	SELECT "NpriID", 
		percentile_cont(0.50) within group (order by "Residential instability Scores") as median_instability_2021,
		percentile_cont(0.50) within group (order by "Economic dependency Scores") as median_dependency_2021,
		percentile_cont(0.50) within group (order by "Ethno-cultural composition Scores") as median_composition_2021,
		percentile_cont(0.50) within group (order by "Situational vulnerability Scores") as median_vulnerability_2021
		FROM (
			SELECT * FROM (SELECT b."NpriID", das.dauid FROM lda_000b21a_e AS das, buffer_1km as b WHERE ST_Within(das.geom, b.st_buffer)) as selection  /* ::int */
			LEFT JOIN cimd_2021 USING ("dauid")
		) as summary
	GROUP BY "NpriID"
	ORDER BY "NpriID";
DROP VIEW IF EXISTS facility_1km_cimd2021;
CREATE VIEW facility_1km_cimd2021 AS
	SELECT * FROM (SELECT "NpriID" FROM facility_da WHERE "NpriID" in (SELECT "NpriID" FROM active_years WHERE "ActiveYears" LIKE '%202%')) as selection /* NOTE: filter to 2020s for CIMD 2016 */
	LEFT JOIN buffer_1km_cimd2021 USING ("NpriID")
	ORDER BY "NpriID";
	
DROP VIEW IF EXISTS cimd_by_id_2021;
CREATE VIEW cimd_by_id_2021 AS
	SELECT * FROM facility_da
	LEFT JOIN cimd_2021 USING ("dauid")
	ORDER BY "NpriID";
ALTER VIEW cimd_by_id_2021 RENAME COLUMN "dauid" TO "fac_dauid";


    
/* Corporate info */
/* Most recent company for each NpriID */
DROP VIEW IF EXISTS company_info_mostrecent CASCADE;
	CREATE VIEW company_info_mostrecent AS
	select * from (
		select distinct on ("NpriID") "NpriID", "CompanyID" as "CompanyId", "FacilityID"
		from NPRI_REPORT order by "NpriID", TO_DATE("SubmitDate", 'YYYY-MM-DD') DESC
	) as lookup
	LEFT JOIN company using ("CompanyId");

/* Most recent corporate owner info */
DROP VIEW IF EXISTS parent_info_mostrecent;
CREATE VIEW parent_info_mostrecent AS
	SELECT DISTINCT ON ("CompanyId")
		"CompanyID" as "CompanyId", 
		"Year" as "MostRecentYear",
		string_agg(DISTINCT "LegalName", ', ') as "Owners",
		string_agg(DISTINCT "Address", '; ') as "Addresses"
	FROM companyparent
	GROUP BY "CompanyId", "MostRecentYear"
	ORDER BY "CompanyId", "MostRecentYear" DESC;
DROP VIEW IF EXISTS corporate_info;
CREATE VIEW corporate_info AS
	select * from company_info_mostrecent LEFT JOIN parent_info_mostrecent using ("CompanyId");

/* Basic info - geographic information plus corporate info */
/* TBD: fill in watersheds and ecozones codes */
DROP VIEW IF EXISTS npri_info CASCADE;
CREATE VIEW npri_info AS
	SELECT * FROM (
		SELECT "NpriId" as "NpriID", "ProvinceID", "CensusSubdivisionID", "EcozoneID", "ForwardSortationArea", "WaterSurveyID", "QuebecWindsorCorridor", geom FROM geographiclocation_unique
	) as geo
	LEFT JOIN corporate_info USING ("NpriID")
	LEFT JOIN (select distinct ("NpriID"), "NAICSPrimary", "NAICSTitleEn" from detail_npri_reports where "ReportYear" = 2022) as j USING ("NpriID");




/* MATERIALIZED VIEWS - what most queries from users will be against
These represent different ways to link NPRI tables, including to Census shapefiles and the CIMD */

/* 
Criteria Air Contaminants (CAC) substance amounts as columns, by id and year
NPRIid	Year	Pollutant X	Pollutant Y	Pollutant Z
123	2001	Null	Null	124
123	2002	100	Null	125
456	2001	Null	Null	500
*/
DROP MATERIALIZED VIEW IF EXISTS npri_reports;
CREATE VIEW npri_reports AS
	SELECT *
	FROM   crosstab(
	   'SELECT * FROM (SELECT CONCAT("NpriID", "ReportYear") as "uid", "NpriID", "ReportYear", "Substance", SUM("SumInTonnes") as "SumInTonnes"
		FROM cac_by_id_year 
		WHERE   "ReportYear" <= 2022 AND "ReportYear" >= 2008
		GROUP BY "uid", "Substance","NpriID", "ReportYear"
		ORDER BY 1,2) as r'
	  , $$VALUES ('Carbon monoxide'), ('Sulphur dioxide'), ('Ammonia (total)'), ('PM10 - Particulate Matter <= 10 Micrometers'), ('PM2.5 - Particulate Matter <= 2.5 Micrometers'), ('Nitrogen oxides (expressed as nitrogen dioxide)'),('Volatile Organic Compounds (Total)') $$
	   ) AS "SumInTonnes" ("uid" text, "NpriID" integer, "ReportYear" integer, "Carbon monoxide" float, "Sulphur dioxide" float, "Ammonia (total)" float, "PM10 - Particulate Matter <= 10 Micrometers" float,
					  "PM2.5 - Particulate Matter <= 2.5 Micrometers" float, "Nitrogen oxides (expressed as nitrogen dioxide)" float, "Volatile Organic Compounds (Total)" float
					  );	


/* Create new table with "empty" reports - those with no facility information */
/* TBD: SUMMARIZE THIS INFORMATION BY MEDIA GROUP, CAS, ETC. */
DROP MATERIALIZED VIEW IF EXISTS npri_gaps CASCADE;
CREATE MATERIALIZED VIEW npri_gaps AS SELECT * FROM detail_npri_reports where "ReportID" is Null;


/* NPRI_EXPORTER - View from **active** facilities, but including previous years data */
DROP MATERIALIZED VIEW IF EXISTS NPRI_EXPORTER;
CREATE MATERIALIZED VIEW NPRI_EXPORTER AS
	select * from active_years /* all NPRI ids ever */
    left join (select * from types_by_id_year where "ReportYear" = 2022) as types_by_id_current using ("NpriID")
    left join cac_by_id_current using ("NpriID")
	left join cac_by_id_fiveyears using ("NpriID")
	left join cac_by_id_fifteenyears using ("NpriID")
	left join cac_by_id_total using ("NpriID")
    left join media_by_id_current using ("NpriID")
	left join air_flags using ("NpriID")
    left join fugitive_by_id_current using ("NpriID")
    left join facility_1km_cimd2021 using ("NpriID")
    left join npri_info using ("NpriID")
    left join facility_da using ("NpriID")
	left join cimd_by_id_2021 using ("NpriID")
	left join facilities_nearby_1km using ("NpriID")
    order by "NpriID";



/* NPRI_SCREEN - View from regions (DAs) */
/* Amount of criteria contaminants reported recently, past 5 years, past 15 years, and total */
DROP VIEW IF EXISTS screen_cac_current;
CREATE VIEW screen_cac_current AS
	select "dauid", sum("Carbon monoxide - Most Recent") as "Carbon monoxide - Most Recent", 
	sum("Sulphur dioxide - Most Recent") as "Sulphur dioxide - Most Recent", 
	sum("Ammonia (total) - Most Recent") as "Ammonia (total) - Most Recent", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - Most Recent") as "PM10 - Particulate Matter <= 10 Micrometers - Most Recent",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent") as "Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent",
	sum("Volatile Organic Compounds (Total) - Most Recent") as "Volatile Organic Compounds (Total) - Most Recent",
	sum("Releases to Air") as "Releases to Air - Most Recent"
	from NPRI_EXPORTER group by "dauid";

DROP VIEW IF EXISTS screen_cac_fiveyears;
CREATE VIEW screen_cac_fiveyears AS
	select "dauid", sum("Carbon monoxide - Past 5 Years") as "Carbon monoxide - Past 5 Years", 
	sum("Sulphur dioxide - Past 5 Years") as "Sulphur dioxide - Past 5 Years", 
	sum("Ammonia (total) - Past 5 Years") as "Ammonia (total) - Past 5 Years", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - Past 5 Years") as "PM10 - Particulate Matter <= 10 Micrometers - Past 5 Years",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 5 Years") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 5 Years",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - Past 5 Years") as "Nitrogen oxides (expressed as nitrogen dioxide) - Past 5 Years",
	sum("Volatile Organic Compounds (Total) - Past 5 Years") as "Volatile Organic Compounds (Total) - Past 5 Years"
	from NPRI_EXPORTER group by "dauid";

DROP VIEW IF EXISTS screen_cac_fifteenyears;
CREATE VIEW screen_cac_fifteenyears AS
	select "dauid", sum("Carbon monoxide - Past 15 Years") as "Carbon monoxide - Past 15 Years", 
	sum("Sulphur dioxide - Past 15 Years") as "Sulphur dioxide - Past 15 Years", 
	sum("Ammonia (total) - Past 15 Years") as "Ammonia (total) - Past 15 Years", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - Past 15 Years") as "PM10 - Particulate Matter <= 10 Micrometers - Past 15 Years",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 15 Years") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - Past 15 Years",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - Past 15 Years") as "Nitrogen oxides (expressed as nitrogen dioxide) - Past 15 Years",
	sum("Volatile Organic Compounds (Total) - Past 15 Years") as "Volatile Organic Compounds (Total) - Past 15 Years"
	from NPRI_EXPORTER group by "dauid";
	
DROP VIEW IF EXISTS screen_cac_total;
CREATE VIEW screen_cac_total AS
	select "dauid", sum("Carbon monoxide - All Years") as "Carbon monoxide - All Years", 
	sum("Sulphur dioxide - All Years") as "Sulphur dioxide - All Years", 
	sum("Ammonia (total) - All Years") as "Ammonia (total) - All Years", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - All Years") as "PM10 - Particulate Matter <= 10 Micrometers - All Years",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - All Years") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - All Years",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - All Years") as "Nitrogen oxides (expressed as nitrogen dioxide) - All Years",
	sum("Volatile Organic Compounds (Total) - All Years") as "Volatile Organic Compounds (Total) - All Years"
	from NPRI_EXPORTER group by "dauid";
/* Cross-check calculations between summarizing from NPRI_EXPORTER and from the detail_npri_reports view */
/* 2022 "46090064"	21.7		3.6	2.67	2.67	29.3		59.94 */
/* select * from (select "DA", sum("Quantity"), "Substance" from detail_npri_reports where "ReportYear" = 2022 and "MediaGroup" = 'Releases to Air' group by "DA", "Substance") as s where "DA" = '46090064'; */
/* TBD: Add percentiles separately in order to not count nulls... */
/* Active Years - Total and Average */
DROP VIEW IF EXISTS screen_activeyears;
CREATE VIEW screen_activeyears AS
	select "dauid", sum("NumberOfActiveYears") as "sum_years",
	avg("NumberOfActiveYears") as "avg_years"
	from NPRI_EXPORTER group by "dauid" order by "avg_years" desc;
/* # of pollutants reported */
/* # of contaminants/media/PAHs/VOCs/CAS etc. */
DROP VIEW IF EXISTS screen_reports;
CREATE VIEW screen_reports AS
	select "DA" as "dauid",
    COUNT (DISTINCT "ReportID") as "NumberOfReports",
    COUNT (DISTINCT "Substance") as "NumberOfSubstances",
    PERCENT_RANK() OVER (ORDER BY COUNT (DISTINCT "Substance")) as "NumberOfSubstances_Percentile",
    COUNT (DISTINCT "MediaGroup") as "NumberOfMedia",
    COUNT (DISTINCT "MeasureMethod") as "NumberOfMeasureMethods",
    COUNT (DISTINCT CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs_Distinct", /* Differentiate DISTINCT and not-distinct. Not-distinct important for cumulative impacts analysis? How to account for "Speciated VOCs"???*/
    COUNT (CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs",
    COUNT (DISTINCT CASE WHEN "IsPAH" THEN "Substance" END) as "NumberOfPAHs",
    string_agg(DISTINCT "Substance", ', ') as "Substances",
    string_agg(DISTINCT "Cas", ', ') as "CAS"
    FROM "detail_npri_reports" where "ReportYear" = 2022
    GROUP BY "dauid"
    ORDER BY "NumberOfSubstances_Percentile" desc;
/* TBD: CEJST-equivalent metrics  - will need to add Census data back in to do this 
Other contextual characteristics?
*/
/* Create NPRI_SCREEN */
DROP MATERIALIZED VIEW IF EXISTS NPRI_SCREEN;
CREATE MATERIALIZED VIEW NPRI_SCREEN AS
	SELECT * FROM cimd_2021
	LEFT JOIN screen_cac_current USING ("dauid")
	LEFT JOIN screen_cac_fiveyears USING ("dauid")
	LEFT JOIN screen_cac_fifteenyears USING ("dauid")
	LEFT JOIN screen_cac_total USING ("dauid")
	LEFT JOIN screen_activeyears USING ("dauid")
	LEFT JOIN screen_reports USING ("dauid")
	LEFT JOIN (select "dauid", "pruid"::int as "ProvinceID", "geom" from lda_000b21a_e) as geo USING ("dauid")
	ORDER BY "dauid";
	
	
	
/* NPRI_COMPANIES – view from current companies/corporate owners */ 
/* TBD: A way of pulling specific owners, separated by commas, out of Owners field */
/* Amount of criteria contaminants currently reported */
DROP VIEW IF EXISTS companies_cac_2022;
CREATE VIEW companies_cac_2022 AS
	select "CompanyId", sum("Carbon monoxide - Most Recent") as "Carbon monoxide - Most Recent", 
	sum("Sulphur dioxide - Most Recent") as "Sulphur dioxide - Most Recent", 
	sum("Ammonia (total) - Most Recent") as "Ammonia (total) - Most Recent", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - Most Recent") as "PM10 - Particulate Matter <= 10 Micrometers - Most Recent",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent") as "Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent",
	sum("Volatile Organic Compounds (Total) - Most Recent") as "Volatile Organic Compounds (Total) - Most Recent",
	sum("Releases to Air") as "Releases to Air - Most Recent"
	from NPRI_EXPORTER group by "CompanyId" order by "Releases to Air - Most Recent" desc;
/*5745 companies ("CompanyIDs") owning 7372 NPRI facilities (NpriIDs) 
select count(distinct("CompanyID")) from detail_npri_reports where "ReportYear" = 2022;
select count(distinct("NpriID")) from detail_npri_reports where "ReportYear" = 2022;
*/
/* Count of NPRI IDs/facilities per company */
DROP VIEW IF EXISTS companies_npris;
CREATE VIEW companies_npris AS
	select "CompanyId", "CompanyName", "Owners", count(distinct("NpriID")) as "CountOfNPRIidsThisCompany" from NPRI_EXPORTER group by "CompanyId","CompanyName","Owners" order by "CountOfNPRIidsThisCompany" desc;
/* # of pollutants reported  */
DROP VIEW IF EXISTS companies_reports;
CREATE VIEW companies_reports AS
select "CompanyID" as "CompanyId",
    COUNT (DISTINCT "ReportID") as "NumberOfReports",
    COUNT (DISTINCT "Substance") as "NumberOfSubstances",
    PERCENT_RANK() OVER (ORDER BY COUNT (DISTINCT "Substance")) as "NumberOfSubstances_Percentile",
    COUNT (DISTINCT "MediaGroup") as "NumberOfMedia",
    COUNT (DISTINCT "MeasureMethod") as "NumberOfMeasureMethods",
    COUNT (DISTINCT CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs_Distinct", /* Differentiate DISTINCT and not-distinct. Not-distinct important for cumulative impacts analysis? How to account for "Speciated VOCs"???*/
    COUNT (CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs",
    COUNT (DISTINCT CASE WHEN "IsPAH" THEN "Substance" END) as "NumberOfPAHs",
    string_agg(DISTINCT "Substance", ', ') as "Substances",
    string_agg(DISTINCT "Cas", ', ') as "CAS"
    FROM "detail_npri_reports" where "ReportYear" = 2022
    GROUP BY "CompanyId"
    ORDER BY "NumberOfSubstances_Percentile" desc;
/* TBD: Demographic */
/* Create NPRI_COMPANIES */
DROP MATERIALIZED VIEW IF EXISTS NPRI_COMPANIES;
CREATE MATERIALIZED VIEW NPRI_COMPANIES AS
	SELECT * FROM companies_cac_2022
	LEFT JOIN companies_npris USING ("CompanyId")
	LEFT JOIN companies_reports USING ("CompanyId")
	ORDER BY "CompanyId";


/* NPRI_INDUSTRY – view across industry type (NAICS) */
/* Amount of pollutants reported */
/* Amount of criteria contaminants reported (Recent and total) */
DROP VIEW IF EXISTS naics_cac_2022;
CREATE VIEW naics_cac_2022 AS
	select "NAICSPrimary", sum("Carbon monoxide - Most Recent") as "Carbon monoxide - Most Recent", 
	sum("Sulphur dioxide - Most Recent") as "Sulphur dioxide - Most Recent", 
	sum("Ammonia (total) - Most Recent") as "Ammonia (total) - Most Recent", 
	sum("PM10 - Particulate Matter <= 10 Micrometers - Most Recent") as "PM10 - Particulate Matter <= 10 Micrometers - Most Recent",
	sum("PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent") as "PM2.5 - Particulate Matter <= 2.5 Micrometers - Most Recent",
	sum("Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent") as "Nitrogen oxides (expressed as nitrogen dioxide) - Most Recent",
	sum("Volatile Organic Compounds (Total) - Most Recent") as "Volatile Organic Compounds (Total) - Most Recent",
	sum("Releases to Air") as "Releases to Air - Most Recent"
	from NPRI_EXPORTER group by "NAICSPrimary" order by "Releases to Air - Most Recent" desc;
/*select * from (select "NAICSPrimary", "NAICSTitleEn", "Substance", sum("Quantity") from detail_npri_reports 
               where "ReportYear" = 2022 group by "NAICSPrimary", "NAICSTitleEn", "Substance") as s;*/
/* # of pollutants reported */
/* # of contaminants/media/PAHs/VOCs/CAS etc. */
DROP VIEW IF EXISTS naics_reports;
CREATE VIEW naics_reports AS
select "NAICSPrimary", "NAICSTitleEn",
    COUNT (DISTINCT "ReportID") as "NumberOfReports",
    COUNT (DISTINCT "Substance") as "NumberOfSubstances",
    PERCENT_RANK() OVER (ORDER BY COUNT (DISTINCT "Substance")) as "NumberOfSubstances_Percentile",
    COUNT (DISTINCT "MediaGroup") as "NumberOfMedia",
    COUNT (DISTINCT "MeasureMethod") as "NumberOfMeasureMethods",
    COUNT (DISTINCT CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs_Distinct", /* Differentiate DISTINCT and not-distinct. Not-distinct important for cumulative impacts analysis? How to account for "Speciated VOCs"???*/
    COUNT (CASE WHEN "IsVoc" THEN "Substance" END) as "NumberOfVOCs",
    COUNT (DISTINCT CASE WHEN "IsPAH" THEN "Substance" END) as "NumberOfPAHs",
    string_agg(DISTINCT "Substance", ', ') as "Substances",
    string_agg(DISTINCT "Cas", ', ') as "CAS" 
    FROM "detail_npri_reports" where "ReportYear" = 2022
    GROUP BY "NAICSPrimary", "NAICSTitleEn"
    ORDER BY "NumberOfSubstances_Percentile" desc;
/* TBD: Demographic */
/* Create NPRI_INDUSTRY */
DROP MATERIALIZED VIEW IF EXISTS NPRI_INDUSTRY;
CREATE MATERIALIZED VIEW NPRI_INDUSTRY AS
	SELECT * FROM naics_cac_2022
	LEFT JOIN naics_reports USING ("NAICSPrimary")
	ORDER BY "NAICSPrimary";


/* NPRI_TRI – view from substances */
/* TBD: Known health effects based on: https://www.canada.ca/en/health-canada/services/chemical-substances/fact-sheets/chemicals-glance.html */
/* TBD: Demographics */
/* Total releases */ /* Number of facilities reporting release */ /* Number of regions released in */
DROP MATERIALIZED VIEW IF EXISTS NPRI_TRI;
CREATE MATERIALIZED VIEW NPRI_TRI AS
	select "SubstanceID", "Substance",
		COUNT (DISTINCT "ReportID") as "NumberOfReports",
		COUNT (DISTINCT (CASE WHEN "ReportYear" = 2022 THEN "ReportID" END)) as "NumberOfReports2022",
		COUNT (DISTINCT (CASE WHEN "ReportYear" > 2017 AND "ReportYear" < 2023  THEN "ReportID" END)) as "NumberOfReportsPast5Years", /* Testing past five years approach */
		COUNT (DISTINCT "NpriID") as "NumberOfNPRIids",
		COUNT (DISTINCT "DA") as "NumberOfDAs"
	FROM "detail_npri_reports" 
	WHERE "Substance" IS NOT NULL /* There are many "null" substance values. This is a result of there being substance reports without corresponding NPRI_REPORTs (associating the substance report with a NPRI ID...) See below. */
	GROUP BY "SubstanceID", "Substance"
	ORDER BY "SubstanceID" desc;


/* NPRI_HISTORY – view over time/for a specific year(s) for facility(s), region(s), company(s), or substance(s) */
/* By substance */
DROP VIEW IF EXISTS history_substance;
CREATE VIEW history_substance AS
	SELECT "ReportYear", "Substance",
		COUNT (DISTINCT "ReportID") as "NumberOfReports",
		SUM("Quantity") as "Quantity",
		"Units"
	FROM detail_npri_reports
	GROUP BY "ReportYear", "Substance", "Units"
	ORDER BY "ReportYear";
/* By company */
DROP VIEW IF EXISTS history_company;
CREATE VIEW history_company AS
	SELECT "ReportYear", "CompanyID",
		COUNT (DISTINCT "ReportID") as "NumberOfReports",
		SUM("Quantity") as "Quantity",
		"Units"
	FROM detail_npri_reports
	GROUP BY "ReportYear", "CompanyID", "Units"
	ORDER BY "ReportYear";
/* By DA */
DROP VIEW IF EXISTS history_da;
CREATE VIEW history_da AS
	SELECT "ReportYear", "DA",
		COUNT (DISTINCT "ReportID") as "NumberOfReports",
		SUM("Quantity") as "Quantity",
		"Units"
	FROM detail_npri_reports
	GROUP BY "ReportYear", "DA", "Units"
	ORDER BY "ReportYear";


/* EXTERNAL EXPORT */
/* TBD: if we are creating MVIEWS as tables in order to export, might as well create them as tables in the first place. */
/* Create  */

CREATE TABLE IF NOT EXISTS npri_gaps_table AS SELECT * FROM npri_gaps;
CREATE TABLE IF NOT EXISTS npri_exporter_table AS SELECT * FROM npri_exporter;
CREATE TABLE IF NOT EXISTS npri_screen_table AS SELECT * FROM npri_screen;
CREATE TABLE IF NOT EXISTS npri_companies_table AS SELECT * FROM npri_companies;
CREATE TABLE IF NOT EXISTS npri_industry_table AS SELECT * FROM npri_industry;
CREATE TABLE IF NOT EXISTS npri_tri_table AS SELECT * FROM npri_tri;
CREATE TABLE IF NOT EXISTS history_substance_table AS SELECT * FROM history_substance;
CREATE TABLE IF NOT EXISTS history_company_table AS SELECT * FROM history_company;
CREATE TABLE IF NOT EXISTS history_da_table AS SELECT * FROM history_da;

/*
upload to Google bucket
import to db
grant read access
*/ 