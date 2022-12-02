USE agr_prod
GO


GO


-- Find stock

IF OBJECT_ID(N'tempdb..#stock_delivery') IS NOT NULL
BEGIN
DROP TABLE #stock_delivery
END
GO
 
CREATE TABLE #stock_delivery
(
 item_id int,
 history_date DATE, 
 stock_delivered DECIMAL(18,4)
)
INSERT INTO #stock_delivery
SELECT
    t.item_id,
    t.history_date,
    t.lag_value AS stock_delivered
FROM (
    SELECT 
        hs.item_id,
        hs.history_date,
        hs.value,
        ISNULL(hs.value - LAG(hs.value) OVER(PARTITION BY hs.item_id ORDER BY hs.history_date),0) AS lag_value,
        hs.description 
    FROM dbo.histories_stock AS hs
) t
WHERE lag_value > 0
--
-- Find sales

SELECT TOP 200 
    hs.item_id,
    YEAR(hs.history_date) AS year,
    MONTH(hs.history_date) AS month,
    SUM(hs.value) AS sale
FROM dbo.histories_sale hs
GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
ORDER BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
GO


IF OBJECT_ID(N'tempdb..#items_date') IS NOT NULL
BEGIN
DROP TABLE #items_date
END
GO
 
CREATE TABLE #items_date
(
 item_id int,
 history_date DATE, 
 sale_value DECIMAL(18,4),
 stock_value DECIMAL(18,4)
)
INSERT INTO #items_date
SELECT --TOP 100
    COALESCE(sale.item_id,stock.item_id) AS item_id,
    COALESCE(sale.history_date,stock.history_date) AS history_date,
    ISNULL(sale.value,0) AS sale_value,
    ISNULL(stock.value,0) AS stock_value
FROM dbo.histories_sale AS sale
    FULL OUTER JOIN dbo.histories_stock stock ON sale.history_date = stock.history_date AND sale.item_id = stock.item_id

GO

IF OBJECT_ID(N'tempdb..#data_frame') IS NOT NULL
BEGIN
DROP TABLE #data_frame
END
GO
 
CREATE TABLE #data_frame
(
 item_id int,
 time_id INT,
 history_date DATE, 
 sale_value DECIMAL(18,4),
 stock_value DECIMAL(18,4),
 stock_delivered DECIMAL(18,4),
 confidence_factor DECIMAL(18,4),
 order_frequency_days INT,
 lead_time_days INT
)

INSERT INTO #data_frame
-- Test items
SELECT
    i.id AS item_id,
    --i.item_no,
    ROW_NUMBER() OVER(PARTITION BY i.id ORDER BY hi.history_date) AS time_id,
    hi.history_date,
    hi.sale_value,
    hi.stock_value,
    ISNULL(sd.stock_delivered,0) AS stock_delivered,
    --i.name,
    --i.article_no,
    --i.closed,
    --i.abc_grouping,
    --l.id AS location_id,
    --l.location_no,
    --l.name,
    --sale.first_sale,
    --sale.last_sale,
    --DATEDIFF(DAY, sale.first_sale, sale.last_sale) AS sale_history_days,
    --DATEDIFF(DAY, sale.first_sale, sale.last_sale) /(1*365) AS sale_history_years,
    id.confidence_factor,
    id.order_frequency_days,
    --id.floating_order_frequency_days,
    id.additional_lead_time_days + ior.lead_time_days AS lead_time_days
    --id.min_stock,
    --id.max_stock,

FROM dbo.items i
    INNER JOIN dbo.locations l ON l.id = i.location_id
    INNER JOIN dbo.item_details id ON id.item_id = i.id
    INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = i.id AND ior.primary_route = 1
    LEFT OUTER JOIN #items_date AS hi ON hi.item_id = i.id
    LEFT OUTER JOIN #stock_delivery AS sd ON sd.item_id = hi.item_id AND sd.history_date = hi.history_date
    
    LEFT OUTER JOIN (
        SELECT 
            item_id, MIN(history_date) first_sale, MAX(history_date) last_sale
        FROM dbo.histories_sale 
        GROUP BY item_id
    ) sale ON sale.item_id = i.id
ORDER BY i.article_no, l.location_no

SELECT DISTINCT model_family_text FROM dbo.forecasts_ex

SELECT 
    t.item_id,
    --MAX(t.time_id,
    MAX(t.history_date) AS last_entry_date,
    dt.year,
    dt.month,
    SUM(t.sale_value) AS sale_value,
    --t.stock_value,
    --t.stock_delivered,
    t.confidence_factor,
    t.order_frequency_days,
    t.lead_time_days
    --t.type   
FROM (
SELECT 
    df.item_id,
    df.time_id,
    df.history_date,
    df.sale_value,
    df.stock_value,
    df.stock_delivered,
    ISNULL(df.confidence_factor,50)/100 AS confidence_factor,
    df.order_frequency_days,
    df.lead_time_days,
     CASE WHEN df.time_id <tt.split*0.8 THEN 'Train' ELSE 'Test' END type
FROM #data_frame AS df
LEFT OUTER JOIN (SELECT MAX(time_id) AS split, item_id FROM #data_frame GROUP BY item_id ) tt ON tt.item_id = df.item_id
WHERE df.history_date IS NOT NULL
) t
LEFT OUTER JOIN dbo.date_table AS dt ON dt.date = t.history_date
GROUP BY t.item_id, dt.year,dt.month, t.confidence_factor, t.order_frequency_days, t.lead_time_days
ORDER BY t.item_id, dt.year,dt.month


GO
;WITH dates AS (
SELECT 
    MIN(date) AS first_dom, 
    MAX(date) AS last_dom,
    COUNT(date) AS no_days,
    CAST(dt.year AS INT) AS year,
    CAST(dt.month AS INT) AS month
    
FROM dbo.date_table AS dt
WHERE date BETWEEN '2018-01-01' AND '2022-09-30'
GROUP BY dt.year, dt.month
)
SELECT
    t.item_id,
    dt.first_dom,
    dt.last_dom,
    dt.no_days,
    dt.year,
    dt.month,
    t.sale,
    ISNULL((SELECT TOP 1 value FROM dbo.histories_stock AS hs WHERE hs.item_id = t.item_id AND hs.history_date >= dt.last_dom ORDER BY hs.history_date ),0) AS stock_eom
FROM (
    SELECT 
        hs.item_id,
        CAST(YEAR(hs.history_date) AS INT) AS year,
        CAST(MONTH(hs.history_date) AS INT) AS month,
        SUM(hs.value) AS sale
    FROM dbo.histories_sale AS hs
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
) t
RIGHT OUTER JOIN dates dt ON dt.month = t.month AND dt.year = t.year
ORDER BY t.item_id, dt.year,dt.month