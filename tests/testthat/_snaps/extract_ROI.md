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
              "value": ["roi_signals", "roi_summary", "roi_coord"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["ROI_1", "ROI_2", "ROI_3"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [12, 1]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [12, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 2, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [54.09634293, 122.93042945, 152.88928548, 125.8530333, 209.341305, 165.00034942, 199.64785438, 141.02425947, 121.79300364, 219.60841659, 111.71476234, 74.86886005]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 1]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [4, 3, 4, 5, 4, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [141.02425947, 111.71476234, 62.42330564, 18.02315702, 84.76064192]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 1]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 2, 6, 7, 7, 7, 8]
                    }
                  },
                  "value": [219.60841659, 232.78336069, 74.86886005, 117.0617602, 214.0572893]
                }
              ]
            },
            {
              "type": "double",
              "attributes": {
                "dim": {
                  "type": "integer",
                  "attributes": {},
                  "value": [1, 3]
                },
                "dimnames": {
                  "type": "list",
                  "attributes": {},
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["frame_1"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI_1", "ROI_2", "ROI_3"]
                    }
                  ]
                },
                "summary": {
                  "type": "character",
                  "attributes": {},
                  "value": ["mean"]
                },
                "area": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI_1", "ROI_2", "ROI_3"]
                    }
                  },
                  "value": [12, 5, 5]
                }
              },
              "value": [141.56399184, 83.58922528, 171.67593737]
            },
            {
              "type": "double",
              "attributes": {
                "dim": {
                  "type": "integer",
                  "attributes": {},
                  "value": [3, 9]
                },
                "dimnames": {
                  "type": "list",
                  "attributes": {},
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI_1", "ROI_2", "ROI_3"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI", "x", "y", "area", "width", "height", "img_width", "img_height", "grain_d"]
                    }
                  ]
                }
              },
              "value": [1, 2, 3, 2, 4, 2, 5, 6, 7, 12, 5, 5, 3, 2, 2, 4, 2, 2, 10, 10, 10, 10, 10, 10, 2, 1, 1]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["extract_ROI"]
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

    {
      "type": "S4",
      "attributes": {
        "data": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["roi_signals", "roi_summary", "roi_coord"]
            }
          },
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["ROI_1", "ROI_2", "ROI_3", "ROI_1", "ROI_2", "ROI_3"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [12, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [12, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 2, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [126.37636235, 146.59241056, 19.65141697, 9.06284779, 174.11096006, 153.3930105, 60.91151283, 65.8323113, 156.73446778, 142.07568241, 83.83821636, 228.24899625, 42.19075296, 88.54382282, 33.51779174, 95.49415046, 78.14303102, 147.48025572, 232.14442758, 36.36404094, 194.30333471, 237.89200636, 120.02301688, 80.87189128, 79.68486241, 251.50945523, 74.869415, 101.83453227, 252.32245753, 44.9952693, 138.24325822, 97.99749247, 123.9680216, 16.26962902, 200.05928865, 84.98588605]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [4, 3, 4, 5, 4, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [65.8323113, 83.83821636, 115.54851859, 127.61244802, 198.8961469, 36.36404094, 120.02301688, 153.91495726, 123.67236855, 200.82282947, 97.99749247, 200.05928865, 106.67201713, 250.15961162, 70.59670963]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 2, 6, 7, 7, 7, 8]
                    }
                  },
                  "value": [142.07568241, 72.62157265, 228.24899625, 113.79000745, 214.35671127, 237.89200636, 133.61644986, 80.87189128, 70.88133752, 27.8024635, 16.26962902, 27.82074568, 84.98588605, 213.54122516, 143.35508088]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [12, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [12, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 2, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [126.37636235, 146.59241056, 19.65141697, 9.06284779, 174.11096006, 153.3930105, 60.91151283, 65.8323113, 156.73446778, 142.07568241, 83.83821636, 228.24899625, 42.19075296, 88.54382282, 33.51779174, 95.49415046, 78.14303102, 147.48025572, 232.14442758, 36.36404094, 194.30333471, 237.89200636, 120.02301688, 80.87189128, 79.68486241, 251.50945523, 74.869415, 101.83453227, 252.32245753, 44.9952693, 138.24325822, 97.99749247, 123.9680216, 16.26962902, 200.05928865, 84.98588605]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [4, 3, 4, 5, 4, 5, 6, 6, 6, 7]
                    }
                  },
                  "value": [65.8323113, 83.83821636, 115.54851859, 127.61244802, 198.8961469, 36.36404094, 120.02301688, 153.91495726, 123.67236855, 200.82282947, 97.99749247, 200.05928865, 106.67201713, 250.15961162, 70.59670963]
                },
                {
                  "type": "double",
                  "attributes": {
                    "dim": {
                      "type": "integer",
                      "attributes": {},
                      "value": [5, 3]
                    },
                    "dimnames": {
                      "type": "list",
                      "attributes": {},
                      "value": [
                        {
                          "type": "NULL"
                        },
                        {
                          "type": "character",
                          "attributes": {},
                          "value": ["frame_1", "frame_2", "frame_3"]
                        }
                      ]
                    },
                    "px_coord": {
                      "type": "integer",
                      "attributes": {
                        "dim": {
                          "type": "integer",
                          "attributes": {},
                          "value": [5, 2]
                        },
                        "dimnames": {
                          "type": "list",
                          "attributes": {},
                          "value": [
                            {
                              "type": "NULL"
                            },
                            {
                              "type": "character",
                              "attributes": {},
                              "value": ["x", "y"]
                            }
                          ]
                        }
                      },
                      "value": [2, 1, 2, 3, 2, 6, 7, 7, 7, 8]
                    }
                  },
                  "value": [142.07568241, 72.62157265, 228.24899625, 113.79000745, 214.35671127, 237.89200636, 133.61644986, 80.87189128, 70.88133752, 27.8024635, 16.26962902, 27.82074568, 84.98588605, 213.54122516, 143.35508088]
                }
              ]
            },
            {
              "type": "double",
              "attributes": {
                "dim": {
                  "type": "integer",
                  "attributes": {},
                  "value": [6, 3]
                },
                "dimnames": {
                  "type": "list",
                  "attributes": {},
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["frame_1", "frame_2", "frame_3", "frame_1", "frame_2", "frame_3"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI_1", "ROI_2", "ROI_3"]
                    }
                  ]
                },
                "summary": {
                  "type": "character",
                  "attributes": {},
                  "value": ["mean", "mean"]
                },
                "area": {
                  "type": "double",
                  "attributes": {},
                  "value": [12, 5, 5, 12, 5, 5]
                }
              },
              "value": [113.9023496, 115.58071021, 122.22829731, 113.9023496, 115.58071021, 122.22829731, 118.34552823, 126.95944262, 145.0970239, 118.34552823, 126.95944262, 145.0970239, 154.21859401, 110.2128297, 97.19451336, 154.21859401, 110.2128297, 97.19451336]
            },
            {
              "type": "double",
              "attributes": {
                "dim": {
                  "type": "integer",
                  "attributes": {},
                  "value": [6, 9]
                },
                "dimnames": {
                  "type": "list",
                  "attributes": {},
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI_1", "ROI_2", "ROI_3", "ROI_1", "ROI_2", "ROI_3"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["ROI", "x", "y", "area", "width", "height", "img_width", "img_height", "grain_d"]
                    }
                  ]
                }
              },
              "value": [1, 2, 3, 1, 2, 3, 2, 4, 2, 2, 4, 2, 5, 6, 7, 5, 6, 7, 12, 5, 5, 12, 5, 5, 3, 2, 2, 3, 2, 2, 4, 2, 2, 4, 2, 2, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 2, 1, 1, 2, 1, 1]
            }
          ]
        },
        "originator": {
          "type": "character",
          "attributes": {},
          "value": ["FUN"]
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

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [1, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["frame_1"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          ]
        },
        "summary": {
          "type": "character",
          "attributes": {},
          "value": ["mean"]
        },
        "area": {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          },
          "value": [12, 5, 5]
        }
      },
      "value": [141.56399184, 83.58922528, 171.67593737]
    }

---

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [3, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["frame_1", "frame_2", "frame_3"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          ]
        },
        "summary": {
          "type": "character",
          "attributes": {},
          "value": ["median"]
        },
        "area": {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          },
          "value": [12, 5, 5]
        }
      },
      "value": [134.22602238, 92.01898664, 99.91601237, 115.54851859, 123.67236855, 106.67201713, 142.07568241, 80.87189128, 84.98588605]
    }

---

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [3, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["frame_1", "frame_2", "frame_3"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          ]
        },
        "summary": {
          "type": "character",
          "attributes": {},
          "value": ["sd"]
        },
        "area": {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          },
          "value": [12, 5, 5]
        }
      },
      "value": [66.06434996, 72.56229566, 76.03246883, 51.30909704, 60.101675, 76.32847063, 66.21383619, 80.6985548, 82.4287002]
    }

---

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [3, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["frame_1", "frame_2", "frame_3"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          ]
        },
        "summary": {
          "type": "character",
          "attributes": {},
          "value": ["sum"]
        },
        "area": {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["ROI_1", "ROI_2", "ROI_3"]
            }
          },
          "value": [12, 5, 5]
        }
      },
      "value": [1366.82819517, 1386.96852246, 1466.73956775, 591.72764116, 634.79721311, 725.4851195, 771.09297003, 551.0641485, 485.97256678]
    }

