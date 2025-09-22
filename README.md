
# 🧬 Clinical Raw Data Issue Tracker

[![R-CMD-check](https://github.com/Geomisgytree/rchk0/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Geomisgytree/rchk0/actions/workflows/R-CMD-check.yaml)
[![tests](https://github.com/Geomisgytree/rchk0/actions/workflows/tests.yaml/badge.svg)](https://github.com/Geomisgytree/rchk0/actions/workflows/tests.yaml)
[![codecov](https://codecov.io/gh/Geomisgytree/rchk0/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Geomisgytree/rchk0)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://geomisgytree.github.io/rchk0/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

A spec-driven data validation platform designed for clinical trial
datasets. Built to help study leads automatically detect, track, and
resolve data issues across multiple raw data transfers. Modular,
extensible, and ready for deployment.

## 📌 Features

- ✅ **Spec-Driven Configuration**: Study leads define all logic via
  Excel spec file—no coding required.
- 🔍 **Automated Checkpoints**: Predefined and custom checks run across
  raw datasets.
- 📊 **Excel Output**: Issues are logged into structured Excel tabs with
  status tracking.
- 🔁 **Multi-Run Support**: Handles repeated raw data transfers with
  versioned issue logs.
- 🧠 **Modular Architecture**: Easily extendable to new studies without
  changing core logic.
- 🖥️ **Optional Shiny UI**: For non-technical users to run checks and
  view results interactively.

## 🧪 Tech Stack

| Layer        | Tools Used                                 |
|--------------|--------------------------------------------|
| Language     | R (dplyr, openxlsx, purrr, haven, stringr) |
| Interface    | Shiny (optional)                           |
| Automation   | GitHub Actions (CI/CD), Docker (optional)  |
| Data Storage | Excel (current), SQLite (optional)         |
| Versioning   | Git + GitHub                               |

## 📁 Project Structure

``` r
kiac-data-tracker/
├── README.md                  # 项目说明文档
├── .gitignore                 # 忽略文件配置
├── spec_template.xlsx         # 通用 spec 模板供 study lead 使用
├── config/                    # 每个 study 的 spec 文件
│   └── J3K-MC-KIAC_spec.xlsx
├── sample_data/               # 示例 raw data 和 spec 文件
│   ├── demo_spec.xlsx
│   └── raw_data.sas7bdat
├── driver/                    # 主 driver 脚本
│   └── run_tracker.R
├── functions/                 # 所有功能函数模块
│   ├── preprocessing.R
│   ├── checkpoints.R
│   ├── excel_writer.R
│   └── utils.R
├── shiny_ui/                  # 可选的 Shiny 界面代码
│   └── app.R
├── tests/                     # 单元测试脚本（推荐使用 testthat）
│   └── test_checkpoints.R
├── logs/                      # 每次运行的日志记录
│   └── run_log_20250920.txt
├── output/                    # 生成的 Excel 输出（不纳入 Git）
└── .github/                   # GitHub Actions 自动化配置
    └── workflows/
        └── ci.yml             # CI/CD 流程定义
```

## 🚀 Quick Start

### 1. Clone the repository

``` bash
git clone https://github.com/yourusername/kiac-data-tracker.git
cd kiac-data-tracker
```

### 2. Install R dependencies

``` r
install.packages(c(
  "dplyr", "openxlsx", "haven", 
  "purrr", "stringr", "readxl"
))
```

### 3. Prepare your spec and raw data

- Use spec_template.xlsx as the starting point.
- Place your raw.sas7bdat files in a designated data folder.

### 4. Run the tracker

- The tracker generates an Excel file with multiple tabs:
  - ReadMe: Overview of the run and parameter settings
  - Issue Log: Cumulative issue tracking table
  - Dataset Tabs: Detailed issue records for each dataset

*Add screenshots or demo video links here to illustrate output.*

## 🧠 Design Philosophy

- Configuration over Code: All study-specific logic resides in the spec
  file.
- Modular Design: Each function is independently testable and reusable.
- User-Centric: Study leads can run checks without writing any code.
- Scalability: Supports versioned runs, state tracking, and new studies
  with minimal changes.

## 🛠️ Future Plans

- Migrate core logic to Python (pandas + openpyxl).
- Replace Excel-based issue tracking with SQLite.
- Integrate an Airflow DAG for scheduled runs.
- Deploy the Shiny UI to RStudio Connect or Posit Workbench.
- Package the toolkit as an R library (kiacTracker).

## 👤 Author

**Shushun Ren**  
Statistical programmer / Biostatistician / Data Engineer · OMSCS
Candidate

🔗 [LinkedIn](https://www.linkedin.com/in/shushunr/)  
📧 <shushunr@umich.edu>

## 📆 Change Log

| Version | Date | Type | Description |
|----|----|----|----|
| v1.2.0 | 2025-08-27 | Added | Driver, spec and raw data workflow setup, applied to KIAC, DSAF and DSAG studies |
| v1.1.0 | 2025-06-11 | Added | Modularize functions and optimize workflows, driver program setup |
| v1.0.0 | 2025-02-13 | Initial | Building checkpoints from scratch. Program setups |
