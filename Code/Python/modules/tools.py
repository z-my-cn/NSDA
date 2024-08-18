from typing import List, Tuple, Any

import matplotlib as mpl
import matplotlib.pyplot as plt
import networkx as nx
from networkx import Graph
import numpy as np
from scipy.cluster.hierarchy import dendrogram


def graph_info(G: Graph, label: str="name", index: bool=True):
    """Print graph information with node, edge and their attributes

    Parameters
    ----------
    G : Graph
        A NetworkX graph
    label : str, optional
        Node attribute name to be used as node label, by default "name"
    index : bool, optional
        Whether to print edge index, by default True
        
    Raises
    ------
    ValueError
        Node does not have attribute {label}
    """
    # print graph information
    if G.name:
        print(G.name, end=" ")
    else:
        print("Graph", end=" ")
    print("with", G.number_of_nodes(), "nodes and", G.number_of_edges(), "edges.", end="\n")
    print("Graph type:     ", end=" ")
    if nx.is_directed(G):
        print("directed", end=" ")
    else:
        print("undirected", end=" ")
    if nx.is_weighted(G):
        print("weighted", end=" ")
    else:
        print("unweighted", end=" ")
    if G.is_multigraph():
        print("multigraph", end=" ")
    print()
    
    if G.number_of_nodes() == 0:
        return
    
    # print node information
    # print node attributes name
    node_attr = []
    print("Node attributes:", end=" ")
    for node in G.nodes(data=True):
        for key in node[1]:
            if key not in node_attr:
                node_attr.append(key)
                print(key, end=" ")
        break
    print()
    
    if G.number_of_edges() == 0:
        return
    
    # print edge information
    # print edge attributes name
    print("Edge attributes:", end=" ")
    for edge in G.edges(data=True):
        for key in edge[2]:
            print(key, end=" ")
        break
    print()
    
    # print edge list: node1 --/-> node2
    MAX_WIDTH_LINE = 80
    MAX_LINE = 6
    
    print("Edge list:")
    if nx.is_directed(G):
        link = "->"
    else:
        link = "--"
    if label in node_attr:
        use_label = True
        max_node_len = max([len(G.nodes[node][label]) for node in G.nodes()])
    else:
        use_label = False
        max_node_len = max([len(str(node)) for node in G.nodes()])
    

    count = 0
    max_count_len = min(len(str(G.number_of_edges() - 1)), 2)
    # remain width for edge list
    remain_width = MAX_WIDTH_LINE
    remain_line = MAX_LINE
    for edge in G.edges(data=True):
        if use_label:
            n1 = G.nodes[edge[0]][label]
            n2 = G.nodes[edge[1]][label]
        else:
            n1 = edge[0]
            n2 = edge[1]
        if index:
            format_output = f"[{count:>{max_count_len}}] {n1:<{max_node_len}} {link} {n2:<{max_node_len}}"
        else:
            format_output = f"{n1:<{max_node_len}} {link} {n2:<{max_node_len}}"
        print(format_output, end="")
        count += 1
        remain_width -= len(format_output+"\t")
        if remain_width < len(format_output+"\t"):
            remain_width = MAX_WIDTH_LINE
            remain_line -= 1
            print()
        else:
            print("\t", end="")
        if remain_line == 0:
            print("...")
            break


def map_color(G: Graph, groupby: str="color", cmap: str="tab10") -> List:
    """Map color to node
    
    Parameters
    ----------
    G : Graph
        A NetworkX graph
    groupby : str, optional
        Node attribute name to be used as groupby, by default "color"
    cmap : str, optional
        Color map name, by default "tab10"
        
    Raises
    ------
    ValueError
        Node does not have attribute {groupby}
        
    Returns
    -------
    List
        List of color
    """
    # get group list
    group_list = []
    for node in G.nodes(data=True):
        if groupby not in node[1]:
            raise ValueError(f"Node {node[0]} does not have attribute {groupby}")
        if node[1][groupby] not in group_list:
            group_list.append(node[1][groupby])
        
    # get color list
    cmap = mpl.colormaps.get_cmap(cmap)
    color_list = [cmap(i) for i in range(len(group_list))]
    # map color to group
    color_map = {}
    for i in range(len(group_list)):
        color_map[group_list[i]] = color_list[i]
    return [color_map[node[1][groupby]] for node in G.nodes(data=True)]


def map_shape(G: Graph, groupby: str="shape") -> List:
    """Map shape to node
    
    Warning: nx.draw_networkx_nodes only supports str type for node_shape, this function is not available
    
    Parameters
    ----------
    G : Graph
        A NetworkX graph
    groupby : str, optional
        Node attribute name to be used as groupby, by default "shape"
        
    Raises
    ------
    ValueError
        Node does not have attribute {groupby} or number of group is greater than number of shape
    
    Returns
    -------
    List
        List of shape
    """
    # get group list
    group_list = []
    for node in G.nodes(data=True):
        if groupby not in node[1]:
            raise ValueError(f"Node {node[0]} does not have attribute {groupby}")
        if node[1][groupby] not in group_list:
            group_list.append(node[1][groupby])
    # set shape list
    shape_list = ["o", "s", "d", "^", "v", "<", ">", "p", "h", "8"]
    
    if len(group_list) > len(shape_list):
        raise ValueError(f"Number of group is greater than number of shape ({len(group_list)} > {len(shape_list)})")
    
    # map shape to group
    shape_map = {}
    for i in range(len(group_list)):
        shape_map[group_list[i]] = shape_list[i % len(shape_list)]
    return [shape_map[node[1][groupby]] for node in G.nodes(data=True)]


def count_kstar(g: Graph, k: int) -> int:
    """Count the number of k-star in a graph
    
    Parameters
    ----------
    g : Graph
        A NetworkX graph
    k : int
        k-star's k value
        
    Raises
    ------
    ValueError
        If k is less than 2
        
    Returns
    -------
    int
        Number of k-star
    """
    if k < 2:
        raise ValueError("k is less than 2")
    
    g_copy = g.copy()
    # remove self-loop
    g_copy.remove_edges_from(nx.selfloop_edges(g_copy))
    nkstar = 0
    # get the nodes with degree greater than k-1
    c_nodes = [node for node, degree in g_copy.degree() if degree > k - 1]
    # count the number of k-star
    for c in c_nodes:
        n = len(list(g_copy.neighbors(c)))
        # count the number of k-star with center node c and n neighbors
        # based on the formula: C(n, k-1)
        nkstar += np.math.factorial(n) / (np.math.factorial(k) * np.math.factorial(n - k))
    
    return int(nkstar)


def count_triangle(g: Graph) -> int:
    """Count the number of triangle in a graph

    Parameters
    ----------
    g : Graph
        A NetworkX graph (undirected)
        
    Raises
    ------
    ValueError
        For directed graph, use nx.triadic_census instead
        
    Returns
    -------
    int
        Number of triangle
    """
    if nx.is_directed(g):
        # tip: use nx.triadic_census
        raise ValueError("For directed graph, use nx.triadic_census instead")
    g_copy = g.copy()
    # remove self-loop
    g_copy.remove_edges_from(nx.selfloop_edges(g_copy))
    triangles = set()
    for node1 in g_copy.nodes():
        for node2 in g_copy.neighbors(node1):
            for node3 in g_copy.neighbors(node2):
                if node3 in g_copy.neighbors(node1):
                    triangles.add(tuple(sorted([node1, node2, node3])))
                    
    return len(triangles)


def cluster_layout(G: Graph, communities: List, scale: int=15, seed: int=42) -> dict:
    """Generate layout for a graph with communities
    
    Reference: https://networkx.org/documentation/stable/auto_examples/drawing/plot_clusters.html
    
    Parameters
    ----------
    G : Graph
        A NetworkX graph
    communities : List
        List of communities, each community is a list of nodes
    scale : int, optional
        Scale factor for positions, by default 15
    seed : int, optional
        Random seed for layout, by default 42
        
    Returns
    -------
    Dict
        A dictionary of node positions
    """
    supergraph = nx.cycle_graph(len(communities))
    superpos = nx.spring_layout(G, scale=scale, seed=seed)
    centers = list(superpos.values())
    pos = {}
    for center, com in zip(centers, communities):
        pos.update(nx.spring_layout(nx.subgraph(G, com), center=center, seed=seed))
    return pos


def create_linkage_matrix(cluster_result: List[Tuple[set, ...]]) -> Tuple[np.ndarray, dict[int, Any]]:
    """Create linkage matrix from cluster result
    
    Parameters
    ----------
    cluster_result : List[Tuple[set, ...]]
        A list of cluster result, each element is a tuple of sets

    Returns
    -------
    Tuple[np.ndarray, dict]
        np.ndarray: 2D array
            A linkage matrix
        dict
            A dictionary of all samples and their index, {index: sample}
    """
    # Get all samples
    all_samples = set()
    for cluster in cluster_result[-1]:
        all_samples.update(cluster)
    all_samples = sorted(all_samples)
    
    # If the first-level result is not a single cluster, add it to the cluster result
    if len(cluster_result[0]) != 1:
        cluster_result = [(all_samples, )] + cluster_result
    
    # Initialize the current clusters
    current_clusters = {}
    for idx, sample in enumerate(all_samples):
        current_clusters[frozenset([sample])] = idx
    # Initialize the linkage matrix
    linkage_matrix = []
    # Initialize the cluster id for the next cluster
    cluster_id = len(all_samples)

    # Initialize the distance, which is the number of merges
    distance = 0
    # Traverse the cluster result from the last step to the first step
    for step in range(len(cluster_result) - 2, -1, -1):
        # Get the clusters in the next step
        next_clusters = cluster_result[step]
        
        # Initialize
        new_cluster_map = {}
        used_clusters = set()

        # Check each cluster in the next step
        for cluster in next_clusters:
            # Find the included clusters in the current step
            included_clusters = []
            for subcluster in cluster_result[step + 1]:
                if frozenset(subcluster).issubset(cluster):
                    included_clusters.append(current_clusters[frozenset(subcluster)])

            # If there are more than one included clusters, merge them
            if len(included_clusters) > 1:
                # Get the two smallest indices
                idx1, idx2 = sorted(included_clusters)[:2]
                new_cluster_map[frozenset(cluster)] = cluster_id
                linkage_matrix.append([idx1, idx2, distance + 1, len(cluster)])
                distance += 1
                cluster_id += 1
                used_clusters.update(included_clusters)
            else:
                # if there is only one included cluster, use it directly
                original_idx = current_clusters[frozenset(cluster)]
                new_cluster_map[frozenset(cluster)] = original_idx

        current_clusters = new_cluster_map

    # Ensure that all clusters are used
    linkage_matrix.sort(key=lambda x: x[2])
    
    all_samples_idx = {}
    # Get the index of all samples
    for idx, sample in enumerate(all_samples):
        all_samples_idx[idx] = sample
    
    return np.array(linkage_matrix, dtype=float), all_samples_idx


def plot_dendrogram(cluster_result: List[Tuple[set, ...]], labels: dict=None, color_cut: int=1, ax=None, **kwargs) -> None:
    """Plot dendrogram from cluster result
    
    Parameters
    ----------
    cluster_result : List[Tuple[set, ...]]
        A list of cluster result, each element is a tuple of sets
    labels : dict
        A dictionary of all samples and their label, by default None
    color_cut : int
        Color cut for dendrogram, means the number of clusters, by default 1
    ax : Axes, optional
        Axes object, by default None
    **kwargs
        Other keyword arguments for dendrogram
    """
    linkage_matrix, all_samples_idx = create_linkage_matrix(cluster_result)
    labels_list = None
    if labels:
        labels_list = []
        # 按照idx的顺序获取label
        for idx in range(len(all_samples_idx)):
            sample = all_samples_idx[idx]
            labels_list.append(labels[sample])
    
    color_threshold = linkage_matrix.shape[0] + 2 - color_cut
    
    dendrogram(
        linkage_matrix, 
        color_threshold=color_threshold,
        labels=labels_list, 
        ax=ax, **kwargs
    )