# all analysis based on the analysis plan would be executed

    Code
      meta_run(meta)
    Output
      $`ae_summary(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any;rel;ser')`
      [1] "results of ae_summary(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_summary(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any;rel;ser')`
      [1] "results of ae_summary(meta = meta, population = \"apat\", observation = \"wk24\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'any')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk24\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'aeosi')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'aeosi')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk24\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'rel')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'rel')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk24\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'ser')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk24', parameter = 'ser')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk24\", "
      

# only the first analysis based on the analysis plan would be executed

    Code
      meta_run(meta, i = 1)
    Output
      $`ae_summary(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any;rel;ser')`
      [1] "results of ae_summary(meta = meta, population = \"apat\", observation = \"wk12\", "
      

# selected analysis based on the analysis plan would be executed

    Code
      meta_run(meta, i = c(1, 3, 5))
    Output
      $`ae_summary(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any;rel;ser')`
      [1] "results of ae_summary(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'any')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      
      $`ae_specific(meta = meta, population = 'apat', observation = 'wk12', parameter = 'aeosi')`
      [1] "results of ae_specific(meta = meta, population = \"apat\", observation = \"wk12\", "
      

