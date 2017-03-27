pkg.env <- new.env()
pkg.env$multi_val <- 0
pkg.env$hole_val <- 1

pkg.env$instance_dim_name <- "instance"

# Arbitrary dim and variable names assumed in code.
pkg.env$node_dim_name <- "node"
pkg.env$part_dim_name <- "part"
pkg.env$part_node_count_var_name <- "part_node_count"
pkg.env$part_type_var_name <- "interior_ring"
pkg.env$node_count_var_name <- "node_count"
pkg.env$geom_container_var_name <- "geometry_container"
pkg.env$instance_var_name <- "instance_name"

# Variables prescribed in the specification.
pkg.env$cf_version <- "CF-1.8"
pkg.env$x_cf_role <- "geometry_x_node"
pkg.env$y_cf_role <- "geometry_y_node"
pkg.env$node_coordinates <- "node_coordinates"
pkg.env$geom_type_attr_name <- "geometry_type"
pkg.env$node_count_attr_name <- "node_count"
pkg.env$part_node_count_attr_name <- "part_node_count"
pkg.env$part_type_attr_name <- "interior_ring"
pkg.env$geometry_container_att_name <- "geometry"
