# check functionality

    
    
     [calc_CosmicDoseRate]
    
     ---------------------------------------------------------
     depth (m)              : 2.78
     density (g cm^-3)      : 1.7
     latitude (N deg.)      : 38.06451
     longitude (E deg.)     : 1.49646
     altitude (m)           : 364
     ---------------------------------------------------------
     total absorber (g cm^-2)       : 472.6
    
     cosmic dose rate (Gy ka^-1)    : 0.1518
      [@sea-level & 55 deg. N G.lat]
    
     geomagnetic latitude (deg.)    : 41.1
    
     cosmic dose rate (Gy ka^-1)    : 0.161 +- 0.0161
      [corrected]                 
     ---------------------------------------------------------
    

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
              "value": ["summary", "args"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "density", "latitude", "longitude", "altitude", "total_absorber.gcm2", "d0", "geom_lat", "dc"]
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
                  "value": [2.78]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.7]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [38.06451]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.49646]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [364]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [472.6]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.1518269]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [41.06850451]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.16100203]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "density", "latitude", "longitude", "altitude", "corr.fieldChanges", "est.age", "half.depth", "error"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2.78]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.7]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [38.06451]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.49646]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [364]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [null]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [10]
                }
              ]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["calc_CosmicDoseRate"]
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

---

    corr.fac: 1  diff.one: 0  alt.fac: 1.281314 
    corr.fac: 1  diff.one: 0  alt.fac: 1.281314 
    
    
     [calc_CosmicDoseRate]
    
     Calculating cosmic dose rate for 2 samples.
    
      depth (m) d0 (Gy/ka) dc (Gy/ka) dc_error (Gy/ka)
    1      2.78     0.1781     0.1850           0.0185
    2      3.12     0.1746     0.1814           0.0181

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
              "value": ["summary", "args"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "d0", "dc", "dc_err", "latitude", "longitude", "altitude", "total_absorber.gcm2", "geom_lat"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["data.frame"]
                },
                "row.names": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 2]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2.78, 3.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.1781, 0.1746]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.185, 0.1814]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.0185, 0.0181]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [28.06451, 28.06451]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.49646, 1.49646]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [364, 364]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [236.3, 265.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [31.358495, 31.358495]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "density", "latitude", "longitude", "altitude", "corr.fieldChanges", "est.age", "half.depth", "error"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2.78, 3.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.7]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [28.06451]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.49646]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [364]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [true]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [20]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [true]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [10]
                }
              ]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["calc_CosmicDoseRate"]
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

---

    
    
     [calc_CosmicDoseRate]
    
     ---------------------------------------------------------
     depth (m)              : 1.78 0.12
     density (g cm^-3)      : 0.7 0.2
     latitude (N deg.)      : 120
     longitude (E deg.)     : 30
     altitude (m)           : 1200
     ---------------------------------------------------------
     total absorber (g cm^-2)       : 127
    
     cosmic dose rate (Gy ka^-1)    : 0.1921
      [@sea-level & 55 deg. N G.lat]
    
     geomagnetic latitude (deg.)    : 59.7
    
     cosmic dose rate (Gy ka^-1)    : 0.2417 +- 0.0242
      [corrected]                 
     ---------------------------------------------------------
    

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
              "value": ["summary", "args"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth.1", "depth.2", "density.1", "density.2", "latitude", "longitude", "altitude", "total_absorber.gcm2", "d0", "geom_lat", "dc"]
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
                  "value": [1.78]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.7]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [30]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1200]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [127]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.19206906]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [59.73652117]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.24172076]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "density", "latitude", "longitude", "altitude", "corr.fieldChanges", "est.age", "half.depth", "error"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.78, 0.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.7, 0.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [30]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1200]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [null]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [10]
                }
              ]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["calc_CosmicDoseRate"]
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

---

    
    
     [calc_CosmicDoseRate]
    
     ---------------------------------------------------------
     depth (m)              : 1.78 0.12
     density (g cm^-3)      : 0.2 0.2
     latitude (N deg.)      : 30
     longitude (E deg.)     : 120
     altitude (m)           : 120
     ---------------------------------------------------------
     total absorber (g cm^-2)       : 38
    
     cosmic dose rate (Gy ka^-1)    : 0.2275
      [@sea-level & 55 deg. N G.lat]
    
     geomagnetic latitude (deg.)    : 18.4
    
     cosmic dose rate (Gy ka^-1)    : 0.2154 +- 0.0215
      [corrected]                 
     ---------------------------------------------------------
    

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
              "value": ["summary", "args"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth.1", "depth.2", "density.1", "density.2", "latitude", "longitude", "altitude", "total_absorber.gcm2", "d0", "geom_lat", "dc"]
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
                  "value": [1.78]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [30]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [38]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.22748768]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [18.4128158]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.21542164]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["depth", "density", "latitude", "longitude", "altitude", "corr.fieldChanges", "est.age", "half.depth", "error"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1.78, 0.12]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [0.2, 0.2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [30]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [120]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [null]
                },
                {
                  "type": "logical",
                  "attributes": {},
                  "value": [false]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [10]
                }
              ]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["calc_CosmicDoseRate"]
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

