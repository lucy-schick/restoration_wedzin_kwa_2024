restoration_principles_pb <- tibble::tribble(
                                                                              ~Principle,                                                                                                                                                                                                                                                                                                                                                                                                                     ~Description,
                                    "Target root causes of habitat and ecosystem change",                                                                                                                                                                                                                                                                                            "Aim at the main factors causing ecosystem decline. Identify and try to reverse the impact of human activities on natural processes.",
                                         "Tailor restoration actions to local potential",                                                                                                                                                                                                                                     "Each section of a river or stream has its own set of natural features due to its geographic location and climate. Design restoration efforts to align with these inherent characteristics.",
                            "Match the scale of restoration to the scale of the problem", "The restoration effort should correspond to the extent of the issue. Localized problems, such as removal of vegetation by a riverbank, need targeted solutions. However, broader impacts like floodplain alteration due to railways, highways, and intensive farming, require numerous actions across the area. Restoring habitats for species that traverse large areas needs planning that covers their entire living range.",
                                                   "Be explicit about expected outcomes",                                                                                                                         "Restoring natural areas is a slow process, and it may take time to see the results. Natural ecosystems are always changing, and a single project might not lead to immediate, noticeable improvements. Setting clear, measurable goals is crucial for understanding the impact of restoration efforts."
                            )

restoration_principles_pb_caption <- "Principles of process-based restoration adapted from Beechie et al. (2010). Developed with the aid of GPT-4 as tracked in this link https://chat.openai.com/share/fb0de9b0-0d79-40f8-aec4-a35ac14c1164."



ncfdc_1998_71a  <- tibble::tribble(
  ~`Sub-Basin`, ~Fish.Values, ~Watershed.Value, ~Level.of.Impact, ~Cumulative.Impacts, ~Rank,
  "Richfield",           6L,               3L,               2L,                  1L,    1L,
    "Emerson",           2L,               8L,               1L,                  2L,    2L,
  "McQuarrie",           7L,               4L,               3L,                  3L,    3L,
     "Barren",           5L,               6L,               4L,                  4L,    4L,
     "Aitken",           8L,               2L,               5L,                  6L,    5L,
      "Byman",           4L,               5L,               6L,                  5L,    6L,
       "Buck",           3L,               1L,               7L,                  7L,    7L,
    "Bulkley",           1L,               7L,               8L,                  8L,    8L
  )


ncfdc_1998_71a_caption <- "Adapted from NCFDC (1998) Table 71a: Sub-Basin priority for restoration based on ranks assigned for
fish values, relative watershed value (basin size, position), level of land-use impacts, and level of cumulative
impacts. For the latter, a lower rank is assigned for a lower level of impact. Highest priority is 1 and lowest is 8."
