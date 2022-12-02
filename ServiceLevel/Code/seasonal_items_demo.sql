IF OBJECT_ID(N'tempdb..#data_frame') IS NOT NULL
BEGIN
DROP TABLE #data_frame
END
GO
 
CREATE TABLE #data_frame
(
 item_id int,
 [year] INT,
 [month] INT, 
 sale DECIMAL(18,4),
  first_entry DATE  
)



;WITH date_items AS (
    SELECT 
        id AS item_id,
        dates.*
    FROM dbo.items AS i
    CROSS JOIN (
        SELECT DISTINCT 
            CAST(dt.year AS INT) AS year,
            CAST(dt.month AS INT) AS month
            FROM dbo.date_table AS dt
        WHERE date BETWEEN '2018-01-01' AND '2022-09-30'
    ) dates
),
sales AS (
    SELECT 
        hs.item_id,
        CAST(YEAR(hs.history_date) AS INT) AS year,
        CAST(MONTH(hs.history_date) AS INT) AS month,
        MIN(hs.history_date) AS first_entry,
        SUM(hs.value) AS sale
    FROM dbo.histories_sale AS hs
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
)
INSERT INTO #data_frame
SELECT 
    di.item_id,
    di.year,
    di.month, 
    ISNULL(s.sale,0) AS sale,
    (SELECT TOP 1 first_entry FROM sales ss WHERE ss.item_id = di.item_id ORDER BY first_entry )
FROM date_items di
LEFT OUTER JOIN sales s ON s.item_id = di.item_id AND s.year = di.year AND s.month = di.month

ORDER BY di.item_id, di.year, di.month




SELECT 
    t.item_id,
    t.year,
    t.month,
    t.sale,
    --t.first_entry,
    vif.seasonality
FROM (
SELECT item_id,
       year,
       month,
       sale,
       first_entry,
       CASE 
            WHEN [year] > YEAR(first_entry) OR ([year] = YEAR(first_entry) AND [month] >= MONTH(first_entry)) THEN 1 ELSE 0 END AS filter
FROM #data_frame
)t
LEFT OUTER JOIN dbo.v_item_forecast AS vif ON vif.item_id = t.item_id
WHERE filter = 1
ORDER BY t.item_id, t.year, t.month

SELECT item_id, vif.seasonality FROM dbo.v_item_forecast AS vif
ORDER BY vif.item_id