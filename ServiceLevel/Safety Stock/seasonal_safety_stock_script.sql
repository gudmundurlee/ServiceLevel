
-- SELECT TOP 100 * FROM dbo_cus.seasonal_safety_stock


If(OBJECT_ID('dbo_cus.seasonal_safety_stock') Is NOT NULL)
Begin
    Drop Table dbo_cus.seasonal_safety_stock
END

CREATE TABLE [dbo_cus].[seasonal_safety_stock] (
    item_id INT,
    [demand_date] DATE,
    value DECIMAL(18,4),
    [description] NVARCHAR(255),
     CONSTRAINT [PK_dbo_cus_seasonal_safety_stock] PRIMARY KEY CLUSTERED 
(
	[item_id] ASC,
	[demand_date] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]


GO




If(OBJECT_ID('tempdb..#seasonal_safety_stock') Is NOT NULL)
Begin
    Drop Table #seasonal_safety_stock
END

CREATE TABLE #seasonal_safety_stock (
    item_id INT,
    [yr] INT,
    [mth] INT,
    entries INT,
    value DECIMAL(18,4)
)
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
        AND fv.demand_date BETWEEN '2022-11-01' AND '2023-10-31'
    GROUP BY fv.item_id, f.seasonality, YEAR(fv.demand_date), MONTH(fv.demand_date)--,CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days)) 
    --ORDER BY fv.item_id, YEAR(fv.demand_date), MONTH(fv.demand_date)
)
INSERT INTO #seasonal_safety_stock
(
    item_id,
    yr,
    mth,
    entries,
    value
)

SELECT 
    t.item_id,
    t.yr,
    t.mth,
    t.entries,
    --id.confidence_factor,
    --t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id) AS forecast_ratio,
    --CEILING(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)) AS monthly_ss,
    --CEILING(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))/(entries*1.0) AS daily_ss,
    --CEILING((t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))*(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END))) AS seasonal_safety_stock_month,
    CEILING(ISNULL((t.forecast_value/NULLIF((SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id),0)),0)*(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)))/(t.entries*1.0) AS seasonal_safety_stock_month
    --CEILING(t.entries*CAST(ISNULL(f.y_sdev,0)*dbo.fn_get_z_score_from_p_value(CASE WHEN id.confidence_factor = 50 THEN 50 ELSE 100*(1-(1-id.confidence_factor/100.0)/2.0) END)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))*(t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))) AS seasonal_safety_stock -- Higher values
    --CEILING(t.entries*CAST(ISNULL(f.sigma,0)*dbo.fn_get_z_score_from_p_value(id.confidence_factor)*((id.order_frequency_days+ior.lead_time_days)/30.0) AS DECIMAL(18,4))/(1.0*(id.order_frequency_days_default+ior.lead_time_days))*(t.forecast_value/(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id))) AS seasonal_safety_stock
    --(SELECT SUM(tt.forecast_value) FROM forecast tt WHERE tt.item_id = t.item_id) AS total_forecast,
    --t.seasonality 
FROM forecast t
    INNER JOIN dbo.forecasts f ON f.item_id = t.item_id
    INNER JOIN dbo.item_details AS id ON id.item_id = f.item_id
    INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = f.item_id AND ior.primary_route = 1
ORDER BY t.item_id, t.yr, t.mth

GO
-- TRUNCATE TABLE dbo_cus.seasonal_safety_stock
INSERT INTO dbo_cus.seasonal_safety_stock
(
    item_id,
    demand_date,
    value,
    [description]
)
SELECT 
    sss.item_id,
    dt.date AS demand_date,
    CAST(sss.value  AS DECIMAL(18,4)) AS [value],
    --CAST(sss.value /(1.0*sss.entries) AS DECIMAL(18,4)) AS [value],
    'Seasonal safety stock' AS [description]
FROM #seasonal_safety_stock AS sss
INNER JOIN dbo.date_table AS dt ON YEAR(dt.date) = sss.yr AND MONTH(dt.date) = sss.mth

GO

SELECT * FROM dbo.chart_elements AS ce

INSERT INTO dbo.chart_elements
(
    id,
    name,
    chart_label,
    description,
    data_element_id,
    starts_stops_today,
    aggregation_calc_func,
    aggregated_over_period,
    hidden_in_charts,
    editable,
    editable_element_id,
    highcharts_series_options
)
VALUES
(   100,       -- id - int
    N'safety_stock_for_order_period_new',     -- name - nvarchar(255)
    N'SAFETY_STOCK_FRO_ORDER_PERIOD_NEW',    -- chart_label - nvarchar(255)
    NULL,    -- description - nvarchar(max)
    -1,       -- data_element_id - int
    N'starts',    -- starts_stops_today - nvarchar(10)
    N'avg', -- aggregation_calc_func - nvarchar(255)
    0, -- aggregated_over_period - bit
    0, -- hidden_in_charts - bit
    0, -- editable - bit
    0,    -- editable_element_id - int
    N'{
      "id":"safety_stock_for_order_period_new",
      "name":"SAFETY_STOCK_ORDER_PERIOD_NEW",
      "data":[],
      "color":"#ed2b2b",
      "type":"line",
      "lineWidth":2,
      "legendIndex":19,
      "zIndex":2,
      "dashStyle":"dash",
      "marker":{"enabled":false}
    }'     -- highcharts_series_options - nvarchar(max)
    )



/* ALTER FUNCTION [dbo].[fn_chart_elements_by_item_id] -- ADD to bottom

     DECLARE @id_safety_stock_for_order_period_new INT = (SELECT id FROM @chart_elements WHERE name = 'safety_stock_for_order_period_new')
    IF @id_safety_stock_for_order_period IS NOT NULL
    BEGIN
        INSERT INTO @t
            SELECT 
                 @id_safety_stock_for_order_period_new AS chart_element_id,
                t.item_id,
                MIN(t.demand_date) AS chart_date,
                CEILING(SUM(t.value)) AS [value],
                t.description
                --AVG(t.value),
                --COUNT(t.demand_date) AS op,
                --CEILING(t.rownum/(t.order_period*1.0)) AS safety_stock_order_period
            FROM 
            (
                SELECT 
                    ROW_NUMBER() OVER (PARTITION BY ssn.item_id ORDER BY ssn.demand_date)  AS rownum,
                    ssn.item_id,
                    ssn.demand_date,
                    ssn.value,
                    ISNULL(id.order_frequency_days,1)+ ISNULL(ior.lead_time_days,0) + ISNULL(id.additional_lead_time_days,0) AS order_period,
                    ssn.description 
                FROM dbo_cus.seasonal_safety_stock AS ssn
                    LEFT OUTER JOIN dbo.item_details AS id ON id.item_id = ssn.item_id
                    LEFT OUTER JOIN dbo.item_order_routes AS ior ON ior.item_id = ssn.item_id AND ior.primary_route = 1
                WHERE ssn.item_id = @item_id AND
                    ssn.demand_date BETWEEN @today AND @stop_date
                --ORDER BY ssn.demand_date
            ) t
            GROUP BY t.item_id, t.description, CEILING(t.rownum/(t.order_period*1.0))

    END
*/


/* -- Could create a view that does this?

DECLARE @today DATE = GETDATE(),
        @stop_date DATE = DATEADD(YEAR, 1, GETDATE())

SELECT TOP 200
    t.item_id,
    MIN(t.demand_date) AS chart_date,
    CEILING(SUM(t.value)) AS [value],
    --CEILING(t.rownum/(t.order_period*1.0)),
    t.description 
FROM (

    SELECT  
        ROW_NUMBER() OVER (PARTITION BY sss.item_id ORDER BY sss.demand_date)  AS rownum,
        sss.item_id,
        sss.demand_date,
        sss.value,
        ISNULL(id.order_frequency_days,1)+ ISNULL(ior.lead_time_days,0) + ISNULL(id.additional_lead_time_days,0) AS order_period,
        sss.description
    FROM dbo_cus.seasonal_safety_stock sss
        INNER JOIN dbo.item_details AS id ON id.item_id = sss.item_id
        INNER JOIN dbo.item_order_routes AS ior ON ior.item_id = sss.item_id AND ior.primary_route = 1
    WHERE sss.demand_date BETWEEN @today AND @stop_date
) t
GROUP BY t.item_id, t.description, CEILING(t.rownum/(t.order_period*1.0))
ORDER BY t.item_id, MIN(t.demand_date)

*/