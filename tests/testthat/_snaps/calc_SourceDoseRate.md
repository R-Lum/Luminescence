# snapshot tests

    {
      "type": "S4",
      "attributes": {
        "data": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["dose.rate", "parameters", "call"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["dose.rate", "dose.rate.error", "date"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.04695031, 0.04694723, 0.04694414, 0.04694106, 0.04693797, 0.04693489, 0.0469318, 0.04692872, 0.04692564, 0.04692255]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.00203666, 0.00203652, 0.00203639, 0.00203626, 0.00203612, 0.00203599, 0.00203585, 0.00203572, 0.00203559, 0.00203545]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Date"]
                    }
                  },
                  "value": [15366, 15367, 15368, 15369, 15370, 15371, 15372, 15373, 15374, 15375]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["source.type", "halflife", "dose.rate.unit"]
                }
              },
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Sr-90"]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [28.9]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["Gy/s"]
                }
              ]
            },
            {
              "type": "language",
              "attributes": {
                "srcref": {
                  "type": "integer",
                  "attributes": {
                    "srcfile": {
                      "type": "environment",
                      "attributes": {
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["srcfilecopy", "srcfile"]
                        }
                      },
                      "value": {}
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["srcref"]
                    }
                  },
                  "value": [7, 3, 7, 30, 3, 30, 7, 7]
                }
              },
              "value": ["calc_SourceDoseRate(measurement.date = \"2012-01-27\", calib.date = \"2014-12-19\", ", "    calib.dose.rate = 0.0438, calib.error = 0.0019, predict = 10)"]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["calc_SourceDoseRate"]
        },
        "info": {
          "type": "list",
          "attributes": {},
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

