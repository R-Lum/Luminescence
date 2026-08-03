# snapshot tests

    
    [analyse_FadingMeasurement()]
    
     n.MC:	 10
     tc:	 3.78e+02 s
    ---------------------------------------------------
    T_0.5 interpolated:	 NA
    T_0.5 predicted:	 4e+11
    g-value:		 5.18 ± 0.67 (%/decade)
    g-value (norm. 2 days):	 6.01 ± 0.68 (%/decade)
    ---------------------------------------------------
    rho':			 3.79e-06 ± 8.17e-07
    log10(rho'):		 -5.42 ± 0.09
    ---------------------------------------------------

---

    {
      "type": "S4",
      "attributes": {
        "data": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["fading_results", "rho_prime", "LxTx_table", "irr.times"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["FIT", "MEAN", "SD", "Q_0.025", "Q_0.16", "Q_0.84", "Q_0.975", "TC", "G_VALUE_2DAYS", "G_VALUE_2DAYS.ERROR", "T_0.5_INTERPOLATED", "T_0.5_PREDICTED", "T_0.5_PREDICTED.LOWER", "T_0.5_PREDICTED.UPPER"]
                },
                "row.names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["G_VALUE_2DAYS"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [5.18210596]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [5.13302635]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.66675603]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [3.9584563]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [4.64866176]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [5.6514852]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [6.05817871]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [378]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [6.01065497]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.67879518]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [395648134314.65747]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [18339400087.880424]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [18339400087.880424]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["FIT", "MEAN", "SD", "Q_0.025", "Q_0.16", "Q_0.84", "Q_0.975"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [3.78528968e-06]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [3.78528968e-06]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [8.16731337e-07]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2.23736678e-06]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [3.23529567e-06]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [4.25551368e-06]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [4.86194295e-06]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["LxTx", "LxTx.Error", "TIMESINCEIRR", "TIMESINCEIRR_NORM", "TIMESINCEIRR_NORM.LOG", "LxTx_NORM", "LxTx_NORM.ERROR"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.98, 0.952, 0.924, 0.912, 0.898, 0.974, 0.8, 0.971, 0.987, 0.913, 0.907, 0.899, 0.884, 0.975, 0.771, 0.984, 0.991, 0.922, 0.927, 0.908, 0.899, 0.985, 0.796, 0.987]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.049, 0.0476, 0.0462, 0.0456, 0.0449, 0.0487, 0.04, 0.04855, 0.04935, 0.04565, 0.04535, 0.04495, 0.0442, 0.04875, 0.03855, 0.0492, 0.04955, 0.0461, 0.04635, 0.0454, 0.04495, 0.04925, 0.0398, 0.04935]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6, 381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6, 381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.00952381, 32.21904762, 48.1047619, 79.83809524, 143.31428571, 1, 2050.74285714, 1.00952381, 1.00952381, 32.21904762, 48.1047619, 79.83809524, 143.31428571, 1, 2050.74285714, 1.00952381, 1.00952381, 32.21904762, 48.1047619, 79.83809524, 143.31428571, 1, 2050.74285714, 1.00952381]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00411657, 1.5081127, 1.68218807, 1.90221017, 2.15628948, 0, 3.31191121, 0.00411657, 0.00411657, 1.5081127, 1.68218807, 1.90221017, 2.15628948, 0, 3.31191121, 0.00411657, 0.00411657, 1.5081127, 1.68218807, 1.90221017, 2.15628948, 0, 3.31191121, 0.00411657]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.00616016, 0.97741273, 0.9486653, 0.93634497, 0.92197125, 1, 0.82135524, 0.99691992, 1.01334702, 0.93737166, 0.9312115, 0.92299795, 0.90759754, 1.00102669, 0.79158111, 1.01026694, 1.0174538, 0.94661191, 0.95174538, 0.93223819, 0.92299795, 1.01129363, 0.81724846, 1.01334702]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.05030801, 0.04887064, 0.04743326, 0.04681725, 0.04609856, 0.05, 0.04106776, 0.049846, 0.05066735, 0.04686858, 0.04656057, 0.0461499, 0.04537988, 0.05005133, 0.03957906, 0.05051335, 0.05087269, 0.0473306, 0.04758727, 0.04661191, 0.0461499, 0.05056468, 0.04086242, 0.05066735]
                }
              ]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6, 381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6, 381.6, 12178.8, 18183.6, 30178.8, 54172.8, 378, 775180.8, 381.6]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["analyse_FadingMeasurement"]
        },
        "info": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": []
            }
          },
          "value": []
        },
        ".uid": {
          "type": "character",
          "attributes": {},
          "value": [null]
        },
        ".pid": {
          "type": "character",
          "attributes": {},
          "value": [null]
        }
      },
      "value": {
        "class": "RLum.Results",
        "package": "Luminescence"
      }
    }

---

    
    [analyse_FadingMeasurement()]
    
     n.MC:	 100
     tc:	 9.2e+01 s
    ---------------------------------------------------
    T_0.5 interpolated:	 NA
    T_0.5 predicted:	 2.3e+02
    g-value:		 124.13 ± 74779.69 (%/decade)
    g-value (norm. 2 days):	 -40.52 ± -30.56 (%/decade)
    ---------------------------------------------------
    rho':			 6.79e-05 ± 0.000266
    log10(rho'):		 -4.17 ± 1.7
    ---------------------------------------------------

---

    {
      "type": "S4",
      "attributes": {
        "data": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["fading_results", "rho_prime", "LxTx_table", "irr.times"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["FIT", "MEAN", "SD", "Q_0.025", "Q_0.16", "Q_0.84", "Q_0.975", "TC", "G_VALUE_2DAYS", "G_VALUE_2DAYS.ERROR", "T_0.5_INTERPOLATED", "T_0.5_PREDICTED", "T_0.5_PREDICTED.LOWER", "T_0.5_PREDICTED.UPPER"]
                },
                "row.names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["G_VALUE_2DAYS"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [124.12599318]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-1619.33011452]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [74779.6904548]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-27458.34128927]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-11586.80921895]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [10176.17878618]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [46280.5025858]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [92]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-40.51662127]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-30.55843727]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [230.99681019]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [206.22344295]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [206.22344295]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["FIT", "MEAN", "SD", "Q_0.025", "Q_0.16", "Q_0.84", "Q_0.975"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00006786]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00006786]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00026601]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-0.00008835]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [-0.00006891]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00008078]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00108387]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["LnLx", "LnLx.BG", "TnTx", "TnTx.BG", "Net_LnLx", "Net_LnLx.Error", "Net_TnTx", "Net_TnTx.Error", "SN_RATIO_LnLx", "SN_RATIO_TnTx", "LxTx", "LxTx.Error", "TIMESINCEIRR", "TIMESINCEIRR_NORM", "TIMESINCEIRR_NORM.LOG", "LxTx_NORM", "LxTx_NORM.ERROR"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 2, 3, 4, 5, 6]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.7817387, 1.7817387, 1.77124675, 1.77124675, 1.7730891, 1.7730891]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.36223144, 0.36223144, 0.33551427, 0.33551427, 0.34004753, 0.34004753]
                },
                {
                  "type": "integer",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.41950726, 1.41950726, 1.43573248, 1.43573248, 1.43304158, 1.43304158]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.34354325, 1.34354325, 1.33898948, 1.33898948, 1.33978642, 1.33978642]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [4.91878533, 4.91878533, 5.2791994, 5.2791994, 5.21423905, 5.21423905]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": ["NA", "NA", "NA", "NA", "NA", "NA"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [94, 94, 92, 92, 92.5, 92.5]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.02173913, 1.02173913, 1, 1, 1.00543478, 1.00543478]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00934003, 0.00934003, 0, 0, 0.00235391, 0.00235391]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.988699, 0.988699, 1, 1, 0.99812576, 0.99812576]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.93578941, 0.93578941, 0.93261767, 0.93261767, 0.93317275, 0.93317275]
                }
              ]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [128, 128, 122, 122, 123, 123]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["analyse_FadingMeasurement"]
        },
        "info": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": []
            }
          },
          "value": []
        },
        ".uid": {
          "type": "character",
          "attributes": {},
          "value": [null]
        },
        ".pid": {
          "type": "character",
          "attributes": {},
          "value": [null]
        }
      },
      "value": {
        "class": "RLum.Results",
        "package": "Luminescence"
      }
    }

