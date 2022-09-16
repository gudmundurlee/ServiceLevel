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
    ( SELECT SUM(value) FROM dbo.v_forecast_select AS vdd WHERE vdd.item_id = i.id AND vdd.demand_date <= DATEADD(d,id.order_frequency_days + ior.lead_time_days, GETDATE()) ) AS forecast_order_period,
    ( SELECT STDEV(value) FROM dbo.v_forecast_select AS vdd WHERE vdd.item_id = i.id AND vdd.demand_date <= DATEADD(d,id.order_frequency_days + ior.lead_time_days, GETDATE()) ) AS sd_order_period
FROM dbo.items AS i
INNER JOIN dbo.locations l ON l.id = i.location_id
INNER JOIN dbo.item_details id ON id.item_id = i.id
INNER JOIN dbo.item_order_routes ior ON ior.item_id = i.id AND ior.primary_route = 1
--LEFT OUTER JOIN dbo.v_daily_demand AS vdd ON vdd.item_id = i.id
LEFT OUTER JOIN dbo.stocks AS s ON s.item_id = i.id
LEFT OUTER JOIN dbo.forecasts f ON f.item_id = i.id --AND f.valid = 1
)
SELECT 
    item_id,
    sd.stock_units AS actual_stocks,
    order_period,
    sd.confidence_factor,
    sd.forecast_order_period AS demand_OP,
    sd.sd_order_period,
    CAST(CEILING( ([dbo].[fn_get_z_score_from_p_value](sd.confidence_factor)* sd.sd_order_period)*SQRT(sd.order_period)) AS DECIMAL(18,4)) AS safety_stock_OP
FROM simulated_data sd
--INNER JOIN dbo.histories_sale hs ON hs.item_id = sd.item_id
WHERE sd.confidence_factor IS NOT NULL