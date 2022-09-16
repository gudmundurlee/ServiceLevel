USE agr_prod
GO
WITH simulated_data AS (
SELECT-- TOP 10 
    i.id AS item_id,
    i.item_no, 
    i.article_no,
    l.location_no,
    l.location_type,
    i.closed,
    ISNULL(s.stock_units,0) AS stock_units,
    id.confidence_factor,
    id.order_frequency_days,
    ior.lead_time_days,
    id.order_frequency_days + ior.lead_time_days AS order_period,
    --DATEADD(d,id.order_frequency_days + ior.lead_time_days, GETDATE()) order_period_date,
    --( SELECT SUM(demand) FROM dbo.v_daily_demand AS vdd WHERE vdd.item_id = i.id AND vdd.demand_date <= DATEADD(d,id.order_frequency_days + ior.lead_time_days, GETDATE()) ) AS demand_order_period,
    --f.*
    ( SELECT SUM(value) FROM dbo.v_forecast_select AS vdd WHERE vdd.item_id = i.id AND vdd.demand_date <= DATEADD(d,id.order_frequency_days + ior.lead_time_days, GETDATE()) ) AS forecast_order_period
    
FROM dbo.items AS i
INNER JOIN dbo.locations l ON l.id = i.location_id
INNER JOIN dbo.item_details id ON id.item_id = i.id
INNER JOIN dbo.item_order_routes ior ON ior.item_id = i.id AND ior.primary_route = 1
--LEFT OUTER JOIN dbo.v_daily_demand AS vdd ON vdd.item_id = i.id
LEFT OUTER JOIN dbo.stocks AS s ON s.item_id = i.id
LEFT OUTER JOIN dbo.forecasts f ON f.item_id = i.id --AND f.valid = 1
)
SELECT * FROM simulated_data sd
--INNER JOIN dbo.histories_sale hs ON hs.item_id = sd.item_id
WHERE sd.item_id = 1353

SELECT
    date AS history_date,
    'Test item 1' AS item,
    ISNULL(hs.value,0) AS sale,
    --ISNULL(st.value,0) AS stock,
    (SELECT TOP 1 value FROM histories_stock st WHERE st.history_date <= date AND st.item_id = 1353 ORDER BY st.history_date DESC ) AS stock,
    dt.day_of_year,
    dt.day_of_month,
    dt.day_of_week
FROM
    dbo.date_table AS dt
    LEFT OUTER JOIN dbo.histories_sale  hs ON hs.history_date = dt.date AND hs.item_id =  1353
    --LEFT OUTER JOIN dbo.histories_stock st ON st.history_date = dt.date AND st.item_id =  1353
WHERE dt.date BETWEEN DATEADD(m,-30,GETDATE()) AND GETDATE()