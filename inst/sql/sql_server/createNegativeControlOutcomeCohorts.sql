if object_id('@results_database_schema.ohdsi_t2dpathway_neg_controls', 'U') is not null
drop table @results_database_schema.ohdsi_t2dpathway_neg_controls;

select
  B.ancestor_concept_id as cohort_definition_id,
  A.person_id as subject_id,
  A.condition_start_date as cohort_start_date,
  coalesce(A.condition_end_date, dateadd(d, 1, A.condition_start_date)) as cohort_end_date
into @results_database_schema.ohdsi_t2dpathway_neg_controls
from @cdm_database_schema.condition_occurrence A
join @cdm_database_schema.concept_ancestor B
  on A.condition_concept_id = B.descendant_concept_id
  and B.ancestor_concept_id in (@negative_control_concept_ids);
