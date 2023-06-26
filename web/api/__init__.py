"""Main module for API objects and endpoints"""

from api.endpoints import get_api_endpoint
from api.objects import (
    Average,
    Celltypes,
    Features,
    FractionDetected,
    Markers,
    Organisms,
    Organs,
    DataSources,
    HighestMeasurement,
    SimilarFeatures,
    SimilarCelltypes,
    CelltypeXOrgan,
)

__all__ = (
    "api_dict",
    "get_api_endpoint",
)

api_dict = {
    "organisms": Organisms,
    "organs": Organs,
    "features": Features,
    "celltypes": Celltypes,
    "average": Average,
    "fraction_detected": FractionDetected,
    "markers": Markers,
    "highest_measurement": HighestMeasurement,
    "similar_features": SimilarFeatures,
    "similar_celltypes": SimilarCelltypes,
    "data_sources": DataSources,
    "celltypexorgan": CelltypeXOrgan,
}
