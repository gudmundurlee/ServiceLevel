    SELECT 
        i.id, 
        i.location_id,
        f.sigma,
        y_mean,
        f.y_sdev,
        f.forc_type,
        i.abc_grouping,
        id.confidence_factor,
        ior.lead_time_days,
        id.order_frequency_days_default,
        f.seasonality,
        ISNULL(f.y_sdev/SQRT(n),0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END) AS monthly_upper,
        ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)/30.0 AS monthly_upper,
        SQRT(ior.lead_time_days + id.order_frequency_days)*((ISNULL(f.y_sdev/SQRT(n),0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))/30.0) AS monthly_upper,
        ((ISNULL(f.y_sdev/SQRT(n),0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))/30.0) AS monthly_upper,
        f.safety_stock_next_7_days/7.0 AS FP_daily_safety_stock,
        CAST(CEILING(SQRT((id.order_frequency_days_default+ ior.lead_time_days)/30.0)*(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)))/30.0  AS DECIMAL(18,4))AS daily_safety_stock,
        --CAST(SQRT((id.order_frequency_days_default+ ior.lead_time_days)/30.0)*(ISNULL(f.sigma,0)*(f.sigma/SQRT(n)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))/30.0 ) AS DECIMAL(18,4)) AS ss_normal,
        CAST(CEILING(SQRT((id.order_frequency_days_default+ ior.lead_time_days)/30.0)*(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)))/30.0  AS DECIMAL(18,4))AS daily_safety_stock2,
        (ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))/30.0 AS monthly_z_score, -- if we use sigma from FP
        --dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)*y_sdev+y_mean AS daily_safety_stock,
        CAST(SQRT(y_mean/n)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END) AS DECIMAL(18,4)) AS poisson_safety_stock
    FROM dbo.forecasts AS f
        INNER JOIN dbo.items i ON i.id = f.item_id
        INNER JOIN dbo.item_details AS id ON id.item_id = f.item_id
        INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = f.item_id AND ior.primary_route = 1
    WHERE f.seasonality <> 1 --AND id.confidence_factor <> 50
    GO
    
    -- Monthly values
    /*
        Forecast pro adjusts the safety stock based on the model fit
        Function that computes z score is not quite correct.

    */
SELECT TOP 100 
    f.item_id,
    f.valid,
    f.forc_type_text,
    f.model_family_text,
    f.model_description,
    f.confidence_factor,
    CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END AS z_score,
    f.seasonality,
    f.n,
    f.y_mean, -- predicted mean
    f.y_sdev,  -- 
    f.sigma, -- model fit error?
    CEILING(f.safety_stock_next_30_days) AS FP_monthly_ss, -- output from forecast prod
    CEILING(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)) AS normal_CI, -- calculated confidence interval
    CEILING(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)) AS normal_PI, -- calculated prediction interval
    CEILING(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)) AS normal_CI_cf, -- new cf confidence interval
    CEILING(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)) AS normal_PI_cf,  -- new cf prediction interval
    CEILING(SQRT(y_mean/n)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)) AS poisson_CI
        
FROM dbo.forecasts_ex AS f
    INNER JOIN dbo.items i ON i.id = f.item_id
    INNER JOIN dbo.item_details AS id ON id.item_id = f.item_id
    INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = f.item_id AND ior.primary_route = 1

GO
    

SELECT 
    hs.item_id,
    ROW_NUMBER() OVER (PARTITION BY hs.item_id ORDER BY YEAR(hs.history_date), MONTH(hs.history_date)) AS time,
    YEAR(hs.history_date) AS yr,
    MONTH(hs.history_date) AS mth,
    SUM(hs.value) AS value
FROM dbo.histories_sale AS hs
WHERE hs.item_id = 9
GROUP BY 
        hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date) 
ORDER BY
        YEAR(hs.history_date), MONTH(hs.history_date)


GO

-- SALE distribution on a daily level
-- Maybe skip forecast to date.
SELECT 
    t.item_id,
    COUNT(t.history_date) AS n,
    MIN(t.history_date) AS first_sale_date,
    --t.last_sale,
    t.avg_sale,
    AVG(t.days_between) AS sale_distribution_mean,
    CAST(STDEV(t.days_between) AS DECIMAL(18,4)) AS sale_distribution_std,
    CASE
        WHEN AVG(t.days_between) BETWEEN 1 AND 7 THEN 'Weekly'
        WHEN AVG(t.days_between) BETWEEN 7 AND 14 THEN '2-Weekly'
        WHEN AVG(t.days_between) BETWEEN 14 AND 21 THEN '3-Weekly'
        WHEN AVG(t.days_between) BETWEEN 21 AND 30 THEN 'Monthly'
        WHEN AVG(t.days_between) > 30 THEN 'More than monthly'
    END AS sale_day_average_group,
    AVG(POWER(t.value-t.avg_sale,2)) mean_square_error,
    CASE
        WHEN AVG(ABS(t.value-t.avg_sale))/t.avg_sale BETWEEN 0 AND 0.25 THEN 'Low'
        WHEN AVG(ABS(t.value-t.avg_sale))/t.avg_sale BETWEEN 0.25 AND 0.5 THEN 'Medium'
        WHEN AVG(ABS(t.value-t.avg_sale))/t.avg_sale BETWEEN 0.5 AND 0.75 THEN 'High'
        WHEN AVG(ABS(t.value-t.avg_sale))/t.avg_sale >= 0.75 THEN 'Extremly High'
    END AS sale_fluctuation,
    ISNULL(fe.seasonality,0) AS seasonality,
    ISNULL(fe.trend_text,'No trend') AS trend,
    (SELECT COUNT(DISTINCT so.history_date) FROM dbo.histories_stock so WHERE so.value = 0 AND so.item_id = t.item_id) AS stockouts
    --AVG(ABS(t.value-t.avg_sale)) AS mean_abs_error,
    --AVG(ABS(t.value-t.avg_sale))/t.avg_sale AS rate1,
    --CASE WHEN AVG(ABS(t.value-t.avg_sale)) = 0 THEN 0 ELSE t.avg_sale/AVG(ABS(t.value-t.avg_sale)) END AS rate2
FROM ( 
    SELECT
        hs.item_id,
        hs.history_date,
        LAG(hs.history_date) OVER(PARTITION BY hs.item_id ORDER BY hs.history_date ASC ) AS last_sale,
        DATEDIFF(DAY, LAG(hs.history_date) OVER(PARTITION BY hs.item_id ORDER BY hs.history_date ASC ) , hs.history_date) days_between, 
        hs.value,
        (SELECT AVG(value) FROM dbo.histories_sale AS hs2 WHERE hs2.item_id = hs.item_id) AS avg_sale
    FROM dbo.histories_sale AS hs
    --WHERE hs.item_id = 9
    ) t
    LEFT OUTER JOIN dbo.forecasts_ex AS fe ON fe.item_id = t.item_id
GROUP BY t.item_id, t.avg_sale, fe.seasonality, fe.trend_text
ORDER BY t.item_id --AVG(ABS(t.value-t.avg_sale))/t.avg_sale 

GO
;WITH forecast AS (
    SELECT --TOP 100 
        fv.item_id,
        YEAR(fv.demand_date) AS yr,
        MONTH(fv.demand_date) AS mth,
        SUM(fv.value) AS forecast_value,
        COUNT(fv.demand_date) AS entries,
        --CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days)) AS SafetyStock_Day,
        f.seasonality
    FROM dbo.forecast_values fv
        INNER JOIN dbo.forecasts_ex f ON f.item_id = fv.item_id
        INNER JOIN dbo.item_details AS id ON id.item_id = f.item_id
        INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = f.item_id AND ior.primary_route = 1
    WHERE f.seasonality = 1
        AND fv.demand_date BETWEEN '2022-10-01' AND '2023-09-30'
    GROUP BY fv.item_id, f.seasonality, YEAR(fv.demand_date), MONTH(fv.demand_date)--,CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days)) 
    --ORDER BY fv.item_id, YEAR(fv.demand_date), MONTH(fv.demand_date)
)

SELECT 
    t.item_id,
    t.yr,
    t.mth,
    t.entries,
    t.forecast_value,
    --CEILING(t.entries*CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))) AS SafetyStock_Month,
    t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id) AS forecast_profile,
    CEILING(t.entries*CAST(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))*(t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))) AS seasonal_safety_stock1,
    CEILING(t.entries*CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))*(t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))) AS seasonal_safety_stock2,
    CEILING(t.entries*CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))*(t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))) AS seasonal_safety_stock3
    ,(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id) AS total_forecast
    --t.seasonality 
FROM forecast t
    INNER JOIN dbo.forecasts f ON f.item_id = t.item_id
    INNER JOIN dbo.item_details AS id ON id.item_id = f.item_id
    INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = f.item_id AND ior.primary_route = 1
ORDER BY t.item_id, t.yr, t.mth