DECLARE @item_id INT = 15
    
    ;WITH sale_count AS (
        SELECT
            item_id,
            --COUNT(DISTINCT history_date) AS n,
            --MIN(hs.history_date) AS first_sale,
            --DATEDIFF(DAY, MIN(hs.history_date), GETDATE()) AS total_n,
            DATEDIFF(DAY, MIN(hs.history_date), GETDATE())- COUNT(DISTINCT hs.history_date) AS freq,
            CAST(0 AS DECIMAL(18,4)) AS [value]
        FROM dbo.histories_sale hs
        WHERE hs.value <> 0
        GROUP BY item_id

        UNION ALL

        SELECT 
            hs.item_id,
            --MIN(hs.history_date) AS first_sale,
            --YEAR(hs.history_date) AS year,
            COUNT(hs.value) AS n,
            hs.value    
        FROM dbo.histories_sale AS hs
        WHERE  hs.value <> 0
        GROUP BY hs.item_id, hs.value

    )

    SELECT 
        sc.item_id,
        SUM(sc.value)/SUM(sc.freq) AS daily_mean,
        SUM(POWER(sc.value,2))/SUM(sc.freq) - POWER(SUM(sc.value)/SUM(sc.freq),2) AS daily_variance
    FROM sale_count sc
    GROUP BY sc.item_id
    Go

-- create table sale_distribution (item_id, freq, value)

/*
 ;WITH histories_sale AS (
 
    SELECT 
        hs.item_id,
        MIN(hs.history_date) min_date,
        YEAR(hs.history_date) AS yr,
        MONTH(hs.history_date) AS mth,
        SUM(hs.value) AS [value]
    FROM dbo.histories_sale AS hs
    WHERE hs.item_id = 15
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
 )
    SELECT
        item_id,
        COUNT(*) AS n,
        COUNT(DISTINCT yr, mth),
        COUNT(DISTINCT min_date),
        MIN(hs.min_date) AS first_sale,
        DATEDIFF(MONTH, MIN(hs.min_date), GETDATE()),
        --DATEDIFF(DAY, MIN(hs.history_date), GETDATE()) AS total_n,
        --DATEDIFF(DAY, MIN(hs.history_date), GETDATE())- COUNT(DISTINCT hs.history_date) AS freq,
        CAST(0 AS DECIMAL(18,4)) AS [value]
    FROM histories_sale hs
    WHERE hs.value <> 0
    GROUP BY item_id

;WITH histories_sale AS (
 SELECT 
    ms.item_id,
    (SELECT MIN(hs.history_date) FROM dbo.histories_sale AS hs WHERE hs.item_id = ms.item_id) AS first_sale,
    COUNT(*) AS freq,
    ms.value    
 FROM (
    SELECT 
        hs.item_id,
        MIN(hs.history_date) min_date,
        YEAR(hs.history_date) AS yr,
        MONTH(hs.history_date) AS mth,
        SUM(hs.value) AS [value]
    FROM dbo.histories_sale AS hs
    WHERE hs.item_id = 15
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
) ms
GROUP BY ms.item_id, ms.value
 )
SELECT 
    t.item_id,
    (SELECT MIN(hs.history_date) FROM dbo.histories_sale AS hs WHERE hs.item_id = t.item_id) AS first_sale,
    
    COUNT(*) 
FROM (
    SELECT DISTINCT 
        hs.item_id, MONTH(hs.history_date) m, YEAR(hs.history_date) y 
    FROM dbo.histories_sale hs
    WHERE hs.value <> 0 
    ) t
WHERE t.item_id = 15
GROUP BY t.item_id
*/        
GO


If(OBJECT_ID('tempdb..#sale_distribution') Is NOT NULL)
Begin
    Drop Table #sale_distribution
END

CREATE TABLE #sale_distribution (
    item_id INT,
    freq INT,
    value DECIMAL(18,4)
)
GO
--TRUNCATE TABLE #sale_distribution

;WITH monthly_sales AS (
    SELECT 
        t.item_id,
        (SELECT MIN(hs.history_date) FROM dbo.histories_sale AS hs WHERE hs.item_id = t.item_id) AS first_sale,
        (SELECT MAX(hs.history_date) FROM dbo.histories_sale AS hs WHERE hs.item_id = t.item_id) AS last_sale,
        COUNT(*) AS freq
    FROM (
        SELECT DISTINCT 
            hs.item_id, MONTH(hs.history_date) m, YEAR(hs.history_date) y 
        FROM dbo.histories_sale hs
        WHERE hs.value <> 0 AND hs.history_date < '2022-11-01'
        ) t
   -- WHERE t.item_id = 15
    GROUP BY t.item_id
)
INSERT INTO #sale_distribution
(
    item_id,
    freq,
    value
)
SELECT 
    ms1.item_id,
    DATEDIFF(MONTH, ms1.first_sale, ms1.last_sale) + 1 - ms1.freq AS freq,
    0 AS [value]
FROM monthly_sales ms1
--ORDER BY ms1.item_id
UNION ALL

SELECT 
    ms2.item_id,
    COUNT(*) AS freq,
    ms2.value    
 FROM (
    SELECT 
        hs.item_id,
        MIN(hs.history_date) min_date,
        YEAR(hs.history_date) AS yr,
        MONTH(hs.history_date) AS mth,
        SUM(hs.value) AS [value]
    FROM dbo.histories_sale AS hs
    WHERE hs.history_date < '2022-11-01'
    GROUP BY hs.item_id, YEAR(hs.history_date), MONTH(hs.history_date)
) ms2
GROUP BY ms2.item_id, ms2.value

GO

SELECT 
    res.item_id,
    res.seasonality,
    res.trend,
    res.monthly_mean,
    res.monthly_variance,
    res.daily_mean,
    res.daily_variance,
    CASE 
        WHEN res.seasonality = 1THEN 'Seasonal'
        WHEN res.daily_variance < 0.1 THEN 'Sparse sales'
        WHEN ABS(res.daily_mean-res.daily_variance) <= 1 THEN 'Poisson'
        WHEN res.daily_variance > res.daily_mean THEN 'Negative Binomial'
        ELSE 'Other'
    END
    ,POWER(res.daily_mean,2)/(res.daily_variance-res.daily_mean) AS size
FROM (

SELECT 
    sd.item_id,
    --SUM(sd.freq) AS n,
    --SUM(sd.value ) AS nsum,
    ISNULL(fe.seasonality,0) AS seasonality,
    ISNULL(fe.trend_text, 'No trend') AS trend,
    CAST(SUM(sd.value )/SUM(sd.freq) AS DECIMAL(18,4)) AS monthly_mean,
    CAST(SUM(POWER(sd.value,2))/SUM(sd.freq) - POWER(SUM(sd.value )/SUM(sd.freq),2) AS DECIMAL(18,4)) AS monthly_variance,
    CAST(SUM(sd.value/30.0 )/SUM(sd.freq) AS DECIMAL(18,4)) AS daily_mean,
    CAST(SUM(POWER(sd.value/30.0,2))/SUM(sd.freq) - POWER(SUM(sd.value/30.0 )/SUM(sd.freq),2) AS DECIMAL(18,4))  AS daily_variance
FROM #sale_distribution AS sd
LEFT OUTER JOIN dbo.forecasts_ex AS fe ON fe.item_id = sd.item_id
GROUP BY sd.item_id, ISNULL(fe.seasonality,0),  ISNULL(fe.trend_text, 'No trend')
) res
ORDER BY res.item_id

SELECT * FROM #sale_distribution AS sd
WHERE sd.item_id = 164