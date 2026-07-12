# ClinRiskR

ClinRiskR 是一个面向小样本临床二分类结局研究的可复现、隐私优先 R
分析工具。

> **仅限科研用途。** 本项目不是医疗器械，不可用于诊断、治疗选择或个体
> 临床决策。

项目源于妊娠期高血压疾病研究流程，但统计引擎采用通用配置。仓库示例数据
全部由程序合成，不对应任何真实患者。

## 核心能力

- 校验 ID、0/1 结局、变量类型、缺失值和建模字段。
- 生成按结局分层的基线表，并明确显示缺失情况。
- 使用同一公式拟合普通 Logistic 与 Firth Logistic 回归。
- 输出表观 AUC、Brier 分数、Youden 阈值、灵敏度、特异度和校准指标。
- 保存 CSV、Excel、图形、配置、R 会话信息和文件校验和。
- 不上传或复制输入数据；默认不导出患者级预测。

## 快速开始

~~~sh
R CMD INSTALL .
Rscript scripts/run_example.R
~~~

也可以在 R 中调用：

~~~r
library(clinriskr)

cohort <- simulate_hdp_data(n = 420, seed = 20260502)
result <- run_clinrisk_analysis(
  cohort,
  default_hdp_config(),
  "results/example"
)
~~~

## 使用自己的数据

把私有数据放入 Git 已忽略的 private-data 目录，复制并修改
config/example_config.json，然后运行：

~~~sh
Rscript scripts/run_analysis.R \
  private-data/cohort.xlsx \
  config/my_config.json \
  results/my-analysis
~~~

结局必须编码为 0（未发生）和 1（发生）。支持 CSV、TSV、XLS 和 XLSX。
只有在确认符合伦理审批和隐私要求后，才应添加
--export-predictions 参数导出逐行预测。

当前版本采用完整病例建模，模型性能来自拟合数据本身，不能视为外部验证或
临床效用证据。详细限制见
[统计方法说明](docs/statistical-methods.md)和[隐私说明](PRIVACY.md)。
