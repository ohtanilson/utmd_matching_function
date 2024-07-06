# using Distributions,Random
# using CSV, DataFrames, DelimitedFiles, Statistics
# using Optim, JLD2, MAT
using DataFramesMeta
using VegaLite, CSV
using JuMP, Ipopt

# load data
utmd_output_hello_work_data_part_and_full_time_monthly_job_category = 
    VegaLite.load("../utmd_matching_function/output/utmd_output_hello_work_data_part_and_full_time_monthly_job_category.rds")
names(utmd_output_hello_work_data_part_and_full_time_monthly_job_category)
utmd_output_hello_work_data_part_and_full_time_monthly_prefecture = 
    VegaLite.load("../utmd_matching_function/output/utmd_output_hello_work_data_part_and_full_time_monthly_prefecture.rds")
names(utmd_output_hello_work_data_part_and_full_time_monthly_prefecture)

# set constant
maxtime = 100.0 
max_iter = 1000 

# test_data = 
#     DataFramesMeta.@chain utmd_output_hello_work_data_part_and_full_time_monthly_job_category begin
#     DataFramesMeta.@subset :year .== 2013
#     DataFramesMeta.@subset :month .== 1
#     DataFramesMeta.@select :year :month :job_kinds :candidate_count :position_count :hire_count :efficiency_implied :hire_elasticity_efficiency_unemployed :coef_AU :coef_AU_V :coef_AU_AU :coef_V :coef_V_V
#     end



function compute_optimal_U(
    data::DataFrame,
    maxtime::Float64,
    max_iter::Int64
    )
    @show industry_num = size(data)[1]
    # data
    U = data.candidate_count[1:industry_num]
    V = data.position_count[1:industry_num]
    H = data.hire_count[1:industry_num]
    A = data.efficiency_implied[1:industry_num]
    dlnM_dlnAU = data.hire_elasticity_efficiency_unemployed[1:industry_num]
    # estimated parameter 
    coef_AU = data.coef_AU[1]
    coef_AU_V = data.coef_AU_V[1]
    coef_AU_AU = data.coef_AU_AU[1]
    # set up 
    model = JuMP.Model(optimizer_with_attributes(Ipopt.Optimizer, "max_cpu_time"=>maxtime))
    set_optimizer_attribute(model, "max_iter", max_iter)
    #global param # initial value list
    @variable(model, optimal_U[i = 1:industry_num] >= 0.0, start = 5000) 
    @variable(model, common_dM_dU, start = 0.1)
    # matching elasticity w.r.t. U, function of U, dM_dAU = (dm/dU) (dU/dAU) = dM_dU/A
    @NLexpression(
        model, 
        dM_dU[i= 1:industry_num], 
        #(coef_AU + coef_AU_V * V[i] + 2 * coef_AU_AU * A[i]*optimal_U[i]) * A[i],
        (dlnM_dlnAU[i] * (H[i]/optimal_U[i]))
        )
    @NLconstraint(model, sum(optimal_U[i] for i = 1:industry_num) == sum(U[i] for i = 1:industry_num))
    @NLconstraint(model, [i=1:industry_num], dM_dU[i] == common_dM_dU)
    #@NLconstraint(model, [i=1:industry_num], optimal_U[i] <= V[i])
    JuMP.@NLobjective(model, Max, 0)
    @time JuMP.optimize!(model)
    optimal_U_computed = JuMP.value.(optimal_U)
    @show size(optimal_U_computed)
    @show JuMP.value.(common_dM_dU)
    JuMP.objective_value(model)
    return optimal_U_computed
end
# optimal_U_computed =
#     compute_optimal_U(
#         test_data,
#         maxtime,
#         max_iter
#         )

function compute_optimal_H(
    data,
    optimal_U_computed
    )
    industry_num = size(data)[1]
    @show size(optimal_U_computed)
    # data
    V = data.position_count[1:industry_num]
    A = data.efficiency_implied[1:industry_num]
    # estimated parameter 
    coef_AU = data.coef_AU[1]
    coef_AU_V = data.coef_AU_V[1]
    coef_AU_AU = data.coef_AU_AU[1]
    coef_V = data.coef_V[1]
    coef_V_V = data.coef_V_V[1]
    # 2nd order polynomial 
    optimal_H =
        coef_AU .* A .* optimal_U_computed .+
        coef_AU_V .* (A .* optimal_U_computed).* V.+
        coef_AU_AU .* (A .* optimal_U_computed).^(2) .+
        coef_V .* V .+
        coef_V_V .* (V).^(2)
    return(optimal_H)
end
# optimal_H_computed =
#     compute_optimal_H(
#         test_data,
#         optimal_U_computed
#         )


# diff_H_and_optimal_H =
#     compute_diff_H_and_optimal_H(
#         test_data,
#         maxtime,
#         max_iter
#         )


# (1) iterate all time across prefectures
time_list = unique(utmd_output_hello_work_data_part_and_full_time_monthly_prefecture.time)
month_list = unique(utmd_output_hello_work_data_part_and_full_time_monthly_prefecture.month)
# initialize
merged_table = DataFramesMeta.@chain utmd_output_hello_work_data_part_and_full_time_monthly_prefecture begin
    DataFramesMeta.@transform(:optimal_U_computed = 9999)
    DataFramesMeta.@transform(:optimal_H_computed = 9999)
    DataFramesMeta.@subset(:time .== 9999)
    DataFramesMeta.@select(:time, :prefecture, :optimal_U_computed, :optimal_H_computed)
    end

for tt in time_list
    data = 
        DataFramesMeta.@chain utmd_output_hello_work_data_part_and_full_time_monthly_prefecture begin
        DataFramesMeta.@subset :time .== tt
        DataFramesMeta.@select :time :prefecture :candidate_count :position_count :hire_count :efficiency_implied :hire_elasticity_efficiency_unemployed :coef_AU :coef_AU_V :coef_AU_AU :coef_V :coef_V_V
        end
    optimal_U_computed =
        compute_optimal_U(
            data,
            maxtime,
            max_iter
            )
    optimal_H_computed =
        compute_optimal_H(
            data,
            optimal_U_computed
            )
    #data.hire_count - optimal_H_computed
    # added optimal H
    @show tt
    temp =
        hcat(data.time, data.prefecture, optimal_U_computed, optimal_H_computed)
    global merged_table = vcat(merged_table, temp) 
end

filename = "../utmd_matching_function/output/utmd_output_hello_work_data_part_and_full_time_monthly_optimal_hire_prefecture.csv"
CSV.write(filename, DataFrame(merged_table, :auto))



# (2) iterate all time across industries 
time_list = unique(utmd_output_hello_work_data_part_and_full_time_monthly_job_category.time)
month_list = unique(utmd_output_hello_work_data_part_and_full_time_monthly_job_category.month)
# initialize
merged_table = DataFramesMeta.@chain utmd_output_hello_work_data_part_and_full_time_monthly_job_category begin
    DataFramesMeta.@transform(:optimal_U_computed = 9999)
    DataFramesMeta.@transform(:optimal_H_computed = 9999)
    DataFramesMeta.@subset(:time .== 9999)
    DataFramesMeta.@select(:time, :job_kinds, :optimal_U_computed, :optimal_H_computed)
    end
for tt in time_list
    data = 
        DataFramesMeta.@chain utmd_output_hello_work_data_part_and_full_time_monthly_job_category begin
        DataFramesMeta.@subset :time .== tt
        DataFramesMeta.@select :time :job_kinds :candidate_count :position_count :hire_count :efficiency_implied :hire_elasticity_efficiency_unemployed :coef_AU :coef_AU_V :coef_AU_AU :coef_V :coef_V_V
        end
    optimal_U_computed =
        compute_optimal_U(
            data,
            maxtime,
            max_iter
            )
    optimal_H_computed =
        compute_optimal_H(
            data,
            optimal_U_computed
            )
    # added optimal H
    @show tt
    temp =
        hcat(data.time, data.job_kinds, optimal_U_computed, optimal_H_computed)
    global merged_table = vcat(merged_table, temp) 
end

filename = "../utmd_matching_function/output/utmd_output_hello_work_data_part_and_full_time_monthly_optimal_hire_job_category.csv"
CSV.write(filename, DataFrame(merged_table, :auto))


