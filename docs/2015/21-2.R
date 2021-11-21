weapon_tbl <- tibble::tribble(
  ~Cost, ~Damage, ~Armor,
  8,     4,       0,
  10,    5,       0,
  25,    6,       0,
  40,    7,       0,
  74,    8,       0
)

armor_tbl <- tibble::tribble(
  ~Cost, ~Damage, ~Armor,
  13,    0,       1,
  31,    0,       2,
  53,    0,       3,
  75,    0,       4,
  102,   0,       5,
  0,     0,       0
)

ring_tbl <- tibble::tribble(
  ~Cost, ~Damage, ~Armor,
  25,    1,       0,
  50,    2,       0,
  100,   3,       0,
  20,    0,       1,
  40,    0,       2,
  80,    0,       3,
  0,     0,       0
)

rings <- rbind(as.data.frame(t(combn(7, 2))), c(7, 7))
names(rings) <- c("ring1", "ring2")

outfits <- tidyr::expand_grid(rings,
                              weapon = 1:5,
                              armor = 1:6)


battle <- function(player_atk, player_armor, player_hp = 100,
                   boss_hp = 103, boss_atk = 9, boss_armor = 2) {
  repeat {
    damage <- max(player_atk - boss_armor, 1)
    boss_hp <- boss_hp - damage

    if (boss_hp <= 0) return(TRUE)

    damage <- max(boss_atk - player_armor, 1)
    player_hp <- player_hp - damage

    if (player_hp <= 0) return(FALSE)

  }
}

pre_battle <- function(ring1, ring2, weapon, armor) {
  outfit <- dplyr::bind_rows(
    ring_tbl[c(ring1, ring2), ],
    weapon_tbl[weapon, ],
    armor_tbl[armor, ]
  )

  if (battle(sum(outfit$Damage), sum(outfit$Armor))) {
    return(NA)
  } else {
    return(sum(outfit$Cost))
  }
}

max(purrr::pmap_dbl(outfits, pre_battle), na.rm = TRUE)
