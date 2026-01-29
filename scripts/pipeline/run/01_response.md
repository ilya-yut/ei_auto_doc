## General Overview

This Exception Indicator (EI) monitors SD delivery document status and related header/item statuses in Sales and Distribution to identify deliveries that meet configurable date, organizational, status, and partner criteria. It provides visibility into delivery status patterns, goods movement status, billing status, and credit-check status, enabling detection of blocked deliveries, delayed processing, and status anomalies that require management attention.

This EI serves as an essential control for delivery and order fulfillment oversight by:
- Filtering delivery data by a configurable time window (BACKDAYS) and by a reference date field (e.g. planned goods movement date, delivery date, picking date)
- Calculating status duration in selected time units (DURATION, DURATION_UNIT) relative to the reference date for age-in-status analysis
- Supporting filtering by sales organization, distribution channel, division, shipping point, route, customer (ship-to, sold-to), and three configurable business partner roles
- Enabling multi-dimensional filtering by delivery, billing, picking, packing, and goods movement statuses (WBSTK, FKSTK, LFSTA, KOSTK, etc.) and by document characteristics (type, category, material, sales office)
- Providing partner enrichment (BP1/BP2/BP3 code, function, name) from VBPA for relationship and accountability visibility

This monitoring supports identification of deliveries stuck in specific statuses, billing blocks, credit blocks, and delayed goods movements. The EI is valuable for order-to-cash reviews, delivery performance analysis, and exception management in SD.

The EI retrieves delivery and status data from SAP SD tables (LIKP – Delivery Header, LIPS – Delivery Item, VBUK – SD Document Header Status, VBUP – SD Document Item Status, VBAK – Sales Document Header), joins to VBAP for order item data and to VBPA and KNA1 for partner and customer names, then filters by all configured parameters and computes duration per delivery line. Results are returned in the T_DATA structure for reporting and alerting.